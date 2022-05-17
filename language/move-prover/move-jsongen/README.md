# move-tsgen

Automatically generate json interface description for Move modules. For instance, if we had a move module looking like:

```move
address TestAddress {
  module TestModule {
      use Std::Signer;
      use Std::ASCII;
      use Std::FixedPoint32;

      const SOME_ERROR:u64 = 1;
      
      struct AccountInfo has key {
          name: String,
          age: u8,
          balance: u64,
          some_index: FixedPoint32,
      }
      public(script) fun register(account: &signer, name: vector<u8>, age: u8, balance: u64, some_index: u64) {
          let account_info = AccountInfo{
              name: name,
              age: age,
              balance: balance,
              some_index: FixedPoint32 {value: some_index},
          };

          let signer_addr = Signer::address_of(account);

          assert!(!exists<AccountInfo>(signer_addr), SOME_ERROR);
          move_to(account, account_info);
      }
  }
}
```

The following json interface file would be generated:

```json
{
  "address": "TestAddress",
  "module": "TestModule",
  "constants": [
    { "name": "SOME_ERROR", "type": "u64", "value": "1" }
  ],
  "structs": [
    {
      "name": "AccountInfo",
      "abilities": [ "key" ],
      "type_params": [],
      "fields": [
        { "name": "name", "type": "Std::ASCII::String" },
        { "name": "age", "type": "u8" },
        { "name": "balance", "type": "u64" },
        { "name": "some_index", "type": "Std::FixedPoint32::FixedPoint32" }
      ]
    }
  ],
  "script_functions": [
    {
      "name": "register",
      "type_params": [],
      "args": [
        { "name": "account", "type": "&signer" },
        { "name": "name", "type": "vector<u8>" },
        { "name": "age", "type": "u8" },
        { "name": "balance", "type": "u64" },
        { "name": "some_index", "type": "u64" }
      ]
    }
  ]
}

```

Using the above as input, language-specific Resource/struct parsers and transaction builders can then be generated 
automatically.

A corresponding `tsgen`, which 
- maps struct declarations to TypeScript classes for compile-time typechecking
- generates transaction senders for script functions in TypeScript 

is being worked on in a separate repo and should be available in a few days.
