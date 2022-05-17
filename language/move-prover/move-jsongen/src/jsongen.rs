// Copyright (c) The Diem Core Contributors
// SPDX-License-Identifier: Apache-2.0

#[allow(unused_imports)]
use log::{debug, info, warn};

use itertools::Itertools;
use move_model::ty::{PrimitiveType, Type};
use move_model::{
    code_writer::CodeWriter,
    emitln,
    model::{
        AbilitySet, FunctionEnv, GlobalEnv, ModuleEnv, ModuleId, NamedConstantEnv, QualifiedId,
        StructEnv, TypeParameter,
    },
    symbol::Symbol,
    ty::TypeDisplayContext,
};
use serde::{Deserialize, Serialize};
use std::borrow::Borrow;
use std::{path::PathBuf, rc::Rc};

/// Options passed into the documentation generator.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(default, deny_unknown_fields)]
pub struct JsongenOptions {
    pub output_directory: String,
}

impl Default for JsongenOptions {
    fn default() -> Self {
        Self {
            output_directory: ".".to_string(),
        }
    }
}

/// The documentation generator.
pub struct Jsongen<'env> {
    options: &'env JsongenOptions,
    env: &'env GlobalEnv,
    /// A list of file names and output generated for those files.
    output: Vec<(String, String)>,
    /// Current code writer.
    writer: CodeWriter,
    /// Current module.
    current_module: Option<ModuleEnv<'env>>,
}

impl<'env> Jsongen<'env> {
    /// Creates a new documentation generator.
    pub fn new(env: &'env GlobalEnv, options: &'env JsongenOptions) -> Self {
        Self {
            options,
            env,
            output: Default::default(),
            writer: CodeWriter::new(env.unknown_loc()),
            current_module: None,
        }
    }

    /// Generate document contents, returning pairs of output file names and generated contents.
    pub fn gen(mut self) -> Vec<(String, String)> {
        for module_env in self.env.get_modules() {
            if !module_env.is_script_module() {
                self.writer = CodeWriter::new(self.env.unknown_loc());
                self.gen_module(&module_env);
                let mut path = PathBuf::from(&self.options.output_directory);
                path.push(module_env.get_source_path());
                let filename = path
                    .with_extension("json")
                    .file_name()
                    .expect("file name")
                    .to_os_string()
                    .to_string_lossy()
                    .to_string();
                self.output.push((filename, self.writer.extract_result()));
            }
        }
        self.output
    }

    fn gen_module(&mut self, module_env: &ModuleEnv<'env>) {
        //assert!(module_env.is_target());
        assert!(!module_env.is_script_module());
        // (Re-) initialize state for this module.
        self.current_module = Some(module_env.clone());

        // opening bracket
        emitln!(self.writer, "{");
        // address
        emitln!(
            self.writer,
            "  \"address\": \"{}\",",
            module_env.self_address().to_hex_literal()
        );
        // module
        let module_name = self.name_string(module_env.get_name().name());
        emitln!(self.writer, "  \"module\": \"{}\",", module_name);

        // constants
        emitln!(self.writer, "  \"constants\": [");
        let num_named_constants = module_env.get_named_constant_count();
        for (i, const_env) in module_env.get_named_constants().enumerate() {
            self.gen_named_constant(&const_env, i + 1 == num_named_constants);
        }
        // closing bracket for constants
        emitln!(self.writer, "  ],");

        // structs
        emitln!(self.writer, "  \"structs\": [");
        let num_structs = module_env.get_struct_count();
        for (i, s) in module_env
            .get_structs()
            .sorted_by(|a, b| Ord::cmp(&a.get_loc(), &b.get_loc()))
            .enumerate()
        {
            self.gen_struct(&s, i + 1 == num_structs);
        }
        // closing bracket for structs
        emitln!(self.writer, "  ],");

        // script functions
        emitln!(self.writer, "  \"script_functions\": [");
        let funs = module_env
            .get_functions()
            .filter(|f| f.is_exposed() && f.is_script())
            .sorted_by(|a, b| Ord::cmp(&a.get_loc(), &b.get_loc()))
            .collect_vec();
        let num_script_funcs = funs.len();
        for (i, f) in funs.iter().enumerate() {
            self.gen_script_function(&f, i + 1 == num_script_funcs);
        }
        // closing bracket for script functions
        emitln!(self.writer, "  ]");

        // closing bracket
        emitln!(self.writer, "}");
    }

    fn gen_named_constant(&self, const_env: &NamedConstantEnv, is_last: bool) {
        // name, type, value
        let last_comma = if is_last { "" } else { "," };
        emitln!(
            self.writer,
            "    {}{}",
            &self.named_constant_display(&const_env),
            last_comma
        );
    }

    /// Generates declaration for named constant
    fn named_constant_display(&self, const_env: &NamedConstantEnv<'_>) -> String {
        let name = self.name_string(const_env.get_name());
        let constant_type_str = self.named_constant_type_str(&const_env.get_type());
        if constant_type_str.is_none() {
            // TODO give better error
            assert!(false, "Unacceptable type for named constant: {}", name);
        }
        format!(
            "{{ \"name\": \"{}\", \"type\": \"{}\", \"value\": \"{}\" }}",
            name,
            self.named_constant_type_str(&const_env.get_type())
                .expect(""),
            const_env.get_value(),
        )
    }

    fn named_constant_type_str(&self, ty: &Type) -> Option<&str> {
        match ty {
            Type::Primitive(primitive) => match primitive {
                PrimitiveType::U8 => Some("u8"),
                PrimitiveType::U64 => Some("u64"),
                PrimitiveType::U128 => Some("u128"),
                PrimitiveType::Bool => Some("bool"),
                PrimitiveType::Address => Some("address"),
                _ => None,
            },
            _ => None,
        }
    }

    /// Generates documentation for a struct.
    fn gen_struct(&self, struct_env: &StructEnv<'_>, is_last: bool) {
        let name = self.name_string(struct_env.get_name());
        let ability_set = struct_env.get_abilities();
        let ability_list_str = self.ability_tokens(ability_set).join(", ");
        emitln!(self.writer, "    {");
        emitln!(self.writer, "      \"name\": \"{}\",", name);
        emitln!(self.writer, "      \"abilities\": [{}],", ability_list_str);
        self.gen_type_params(
            "      ",
            &struct_env.get_type_parameters(),
            Some(struct_env),
        );
        self.gen_struct_fields("      ", struct_env);
        let last_comma = if is_last { "" } else { "," };
        emitln!(self.writer, "    }}{}", last_comma);
    }

    fn gen_type_params(
        &self,
        indent: &str,
        type_params: &Vec<TypeParameter>,
        struct_env: Option<&StructEnv>,
    ) {
        let num_params = type_params.len();
        if num_params == 0 {
            emitln!(self.writer, "{}\"type_params\": [],", indent);
        } else {
            emitln!(self.writer, "{}\"type_params\": [", indent);
            for (i, tparam) in type_params.iter().enumerate() {
                let name = self.name_string(tparam.0);
                let abilities = self.ability_tokens(tparam.1 .0).join(", ");
                let is_phantom =
                    if struct_env.is_some() && struct_env.unwrap().is_phantom_parameter(i) {
                        "true"
                    } else {
                        "false"
                    };
                let last_comma = if i + 1 == num_params { "" } else { "," };
                emitln!(
                    self.writer,
                    "{}  {{ \"name\": \"{}\", \"abilities\": [{}], \"is_phantom\": {} }}{}",
                    indent,
                    name,
                    abilities,
                    is_phantom,
                    last_comma
                );
            }
            emitln!(self.writer, "{}],", indent)
        }
    }

    fn gen_struct_fields(&self, indent: &str, struct_env: &StructEnv<'_>) {
        let tctx = self.type_display_context_for_struct(struct_env);
        let num_fields = struct_env.get_field_count();
        let type_params = struct_env.get_type_parameters();
        if num_fields == 0 {
            emitln!(self.writer, "{}\"fields\": []");
        } else {
            emitln!(self.writer, "{}\"fields\": [", indent);
            for (i, field) in struct_env.get_fields().enumerate() {
                let type_str = self.type_to_type_name_str(&field.get_type(), &tctx, &type_params);
                let name_str = self.name_string(field.get_name());
                let is_last = i + 1 == num_fields;
                self.emit_indented_name_type(indent, name_str.as_str(), type_str.as_str(), is_last)
            }
            emitln!(self.writer, "{}]", indent);
        }
    }

    fn emit_indented_name_type(&self, indent: &str, name_str: &str, type_str: &str, is_last: bool) {
        emitln!(
            self.writer,
            &format!(
                "{}  {{ \"name\": \"{}\", \"type\": \"{}\" }}{}",
                indent,
                name_str,
                type_str,
                if is_last { "" } else { "," }
            )
        );
    }

    fn type_to_type_name_str(
        &self,
        ty: &Type,
        tdc: &TypeDisplayContext,
        type_params: &Vec<TypeParameter>,
    ) -> String {
        match ty {
            Type::Primitive(primitive_type) => match primitive_type {
                PrimitiveType::Bool => String::from("bool"),
                PrimitiveType::U8 => String::from("u8"),
                PrimitiveType::U64 => String::from("u64"),
                PrimitiveType::U128 => String::from("u128"),
                PrimitiveType::Address => String::from("address"),
                PrimitiveType::Signer => String::from("signer"),
                _ => String::from("unknown"),
            },
            Type::Vector(type_box) => {
                let inner_ty = type_box.borrow();
                let inner_ty_str = self.type_to_type_name_str(inner_ty, tdc, type_params);
                format!("vector<{}>", inner_ty_str)
            }
            Type::Struct(module_id, struct_id, inner_types) => {
                // this struct may or may not be internally defined, so check first
                let struct_env = self.env.get_struct(QualifiedId {
                    module_id: *module_id,
                    id: *struct_id,
                });
                let struct_name = self.name_string(struct_env.get_name());
                let type_param_str = if inner_types.is_empty() {
                    String::from("")
                } else {
                    format!(
                        "<{}>",
                        inner_types
                            .iter()
                            .map(|t| self.type_to_type_name_str(t, tdc, type_params))
                            .join(",")
                    )
                };
                /*
                if *module_id == self.current_module.as_ref().unwrap().get_id() {
                    // internally defined struct
                    format!("{}{}", struct_name, type_param_str)
                } else {
                 */
                // externally defined struct
                let extern_module_name = self.get_imported_module_name(module_id);
                format!("{}::{}{}", extern_module_name, struct_name, type_param_str)
                //}
            }
            Type::TypeParameter(param_idx) => {
                let type_param: &TypeParameter = &type_params[*param_idx as usize];
                let name = self.name_string(type_param.0);
                name.to_string()
            }
            Type::Reference(_bool, type_box) => {
                let inner_ty: &Type = type_box.borrow();
                if *inner_ty == Type::Primitive(PrimitiveType::Signer) {
                    String::from("&signer")
                } else {
                    String::from("UNSUPPORTED_REFERENCE_TYPE")
                }
            }
            _ => String::from("UNSUPPORTED_TYPE"),
        }
    }

    fn get_imported_module_name(&self, module_id: &ModuleId) -> String {
        let module_env = self.env.get_module(*module_id);
        let address = module_env.self_address().to_hex_literal();
        let mod_name = self.name_string(module_env.get_name().name());
        format!("{}::{}", address, mod_name)
    }

    /// Generates documentation for a function.
    fn gen_script_function(&self, func_env: &FunctionEnv<'_>, is_last: bool) {
        let is_script = func_env.is_script();
        assert!(is_script);
        let name = self.name_string(func_env.get_name());
        emitln!(&self.writer, "    {");
        emitln!(&self.writer, "      \"name\": \"{}\",", name);
        self.gen_type_params("      ", &func_env.get_type_parameters(), None);
        self.gen_func_params("      ", func_env);

        let last_comma = if is_last { "" } else { "," };
        emitln!(&self.writer, "    }}{}", last_comma);
    }

    fn gen_func_params(&self, indent: &str, func_env: &FunctionEnv<'_>) {
        let tctx = self.type_display_context_for_function(func_env);
        let num_params = func_env.get_parameter_count();
        if num_params == 0 {
            emitln!(self.writer, "{}\"params\": []");
        } else {
            emitln!(self.writer, "{}\"params\": [", indent);
            let type_params = func_env.get_type_parameters();
            for (i, param) in func_env.get_parameters().iter().enumerate() {
                let name_str = self.name_string(param.0);
                let type_str = self.type_to_type_name_str(&param.1, &tctx, &type_params);
                let is_last = i + 1 == num_params;
                self.emit_indented_name_type(indent, name_str.as_str(), type_str.as_str(), is_last)
            }
            emitln!(self.writer, "{}]", indent);
        }
    }

    // ============================================================================================
    // Helpers

    /// Returns a string for a name symbol.
    fn name_string(&self, name: Symbol) -> Rc<String> {
        self.env.symbol_pool().string(name)
    }

    /// Collect tokens in an ability set
    fn ability_tokens(&self, abilities: AbilitySet) -> Vec<&'static str> {
        let mut ability_tokens = vec![];
        if abilities.has_copy() {
            ability_tokens.push("\"copy\"");
        }
        if abilities.has_drop() {
            ability_tokens.push("\"drop\"");
        }
        if abilities.has_store() {
            ability_tokens.push("\"store\"");
        }
        if abilities.has_key() {
            ability_tokens.push("\"key\"");
        }
        ability_tokens
    }

    /// Creates a type display context for a struct.
    fn type_display_context_for_struct(
        &self,
        struct_env: &StructEnv<'_>,
    ) -> TypeDisplayContext<'_> {
        let type_param_names = Some(
            struct_env
                .get_named_type_parameters()
                .iter()
                .map(|TypeParameter(name, _)| *name)
                .collect_vec(),
        );
        TypeDisplayContext::WithEnv {
            env: self.env,
            type_param_names,
        }
    }

    /// Creates a type display context for a function.
    fn type_display_context_for_function(
        &self,
        fun_env: &FunctionEnv<'_>,
    ) -> TypeDisplayContext<'_> {
        let type_param_names = Some(
            fun_env
                .get_named_type_parameters()
                .iter()
                .map(|TypeParameter(name, _)| *name)
                .collect_vec(),
        );
        TypeDisplayContext::WithEnv {
            env: self.env,
            type_param_names,
        }
    }
}
