mod ast_exp;
mod ast_tests;
pub mod ast_to_ts;
mod shared;
pub mod tsgen_writer;
pub mod utils;

use crate::shared::is_same_package;
use crate::utils::{generate_index, generate_topmost_index};
use clap::Parser;
use move_command_line_common::address::NumericalAddress;
use move_command_line_common::parser::NumberFormat;
use move_compiler::diagnostics::unwrap_or_report_diagnostics;
use move_compiler::shared::PackagePaths;
use move_compiler::*;
use move_package::compilation::package_layout::CompiledPackageLayout;
use move_package::source_package::layout::SourcePackageLayout;
use shared::{Context, MoveToTsOptions};
use std::collections::BTreeMap;
use std::path::{Path, PathBuf};
use std::process;

fn write_file(root_path: &PathBuf, pair: (String, String)) {
    let (filename, content) = pair;
    let path_to_save = root_path.join(filename);
    std::fs::write(path_to_save, content).expect("Failed to write file to output");
}

fn build(path: &Path, config: &MoveToTsOptions) {
    let build_config = move_package::BuildConfig::default();
    let resolution_graph = build_config
        .resolution_graph_for_package(path)
        .expect("Failed to build resolution graph for package");
    /*
    1. Go through the compilation pipeline to report diagnostics if any, otherwise retain AST
       from the typing stage
    2. feed typing AST through move-tsgen to get files
    3. write files
    4. write jest .test.ts files if --test is given
    5. write package.json and tsconfig.json if --generate-package is given
    6. generate various index.ts for packages
     */
    let root_package = &resolution_graph.package_table[&resolution_graph.root_package.package.name];
    let project_root = match &resolution_graph.build_options.install_dir {
        Some(under_path) => under_path.clone(),
        None => resolution_graph.root_package_path.clone(),
    };

    // 1
    let mut named_address_mapping = BTreeMap::new();

    resolution_graph
        .extract_named_address_mapping()
        .for_each(|(name, addr)| {
            named_address_mapping.insert(
                name,
                NumericalAddress::new(addr.into_bytes(), NumberFormat::Hex),
            );
        });

    let mut dependencies = resolution_graph
        .package_table
        .iter()
        .filter_map(|(name, package)| {
            if name == &root_package.source_package.package.name {
                None
            } else {
                let paths = format!("{}/sources", package.package_path.to_string_lossy());
                let path = PackagePaths {
                    name: Some(*name),
                    paths: vec![paths],
                    named_address_map: named_address_mapping.clone(),
                };
                Some(path)
            }
        })
        .collect::<Vec<_>>();

    let sources = vec![format!(
        "{}/sources",
        root_package.package_path.to_string_lossy()
    )];

    let mut source_package_paths = vec![PackagePaths {
        name: Some(root_package.source_package.package.name),
        paths: sources,
        named_address_map: named_address_mapping,
    }];

    source_package_paths.append(&mut dependencies);

    let flags = if config.test {
        Flags::testing()
    } else {
        Flags::empty()
    };

    // mark everything as source to avoid all functions in dependencies being marked as "native"
    let compiler =
        Compiler::from_package_paths(source_package_paths.clone(), vec![]).set_flags(flags);

    let (files, res_comments_compiler) = compiler
        .run::<{ move_compiler::PASS_TYPING }>()
        .expect("Compilation failed");

    let (_comments, typing_compiler) = unwrap_or_report_diagnostics(&files, res_comments_compiler);

    let (_, typing_program) = typing_compiler.into_ast();

    // run the full pipeline to check errors/warnings
    // move package doesn't provide a way to save intermediate program ast, so rerunning the
    // entire pipeline to check all errors. We need to make a PR upstream to avoid doing repeat
    // work here.

    let compiler = Compiler::from_package_paths(source_package_paths, vec![]);
    let (_, full_res) = compiler
        .run::<{ move_compiler::PASS_COMPILATION }>()
        .expect("Compilation failed");
    unwrap_or_report_diagnostics(&files, full_res);

    // 2 & 3
    let build_root_path = project_root
        .join(CompiledPackageLayout::Root.path())
        .join("typescript");
    let mut ctx = Context::new(config);
    for (mident, mdef) in typing_program.modules.key_cloned_iter() {
        // 2
        let result = ast_to_ts::translate_module(mident, mdef, &mut ctx);

        let (filename, content) = unwrap_or_report_diagnostics(&files, result);

        // 3
        let path_to_save = build_root_path.join("src").join(filename);
        let parent = path_to_save.parent().unwrap();
        std::fs::create_dir_all(&parent).expect("Failed to create directory");
        std::fs::write(path_to_save, content).expect("Failed to write file to output");

        // 4 tests
        if config.test {
            let test_res = ast_tests::generate_tests(&mut ctx);
            let (filename, content) = unwrap_or_report_diagnostics(&files, test_res);
            write_file(&build_root_path.join("src"), (filename, content));
        }
    }

    // 5
    if !config.package_json_name.is_empty() {
        // package.json
        let (filename, content) = utils::generate_package_json(config.package_json_name.clone());
        write_file(&build_root_path, (filename, content));

        // tsconfig.json
        let (filename, content) = utils::generate_ts_config();
        write_file(&build_root_path, (filename, content));
    }

    // 6
    for (package_name, address) in ctx.visited_packages.iter() {
        let modules = ctx
            .visited_modules
            .iter()
            .filter(|mi| is_same_package(mi.value.address, *address))
            .collect::<Vec<_>>();

        let (filename, content) = generate_index(package_name, &modules);
        write_file(&build_root_path.join("src"), (filename, content));
    }

    let package_names = ctx.visited_packages.keys().collect::<Vec<_>>();
    write_file(
        &build_root_path.join("src"),
        generate_topmost_index(&package_names),
    )
}

fn main() {
    let args = MoveToTsOptions::parse();

    let root = SourcePackageLayout::try_find_root(&args.package_path);
    if root.is_err() {
        println!("Please provide path to valid move package or run this command from within one");
        process::exit(-1);
    }
    let root_path = root.unwrap();
    std::env::set_current_dir(&root_path).unwrap();
    println!("Working from {}", root_path.to_string_lossy());
    build(&root_path, &args);
}
