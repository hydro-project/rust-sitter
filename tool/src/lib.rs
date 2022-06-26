use std::path::Path;

pub fn generate_grammar(root_file: &Path) -> std::io::Result<String> {
    let root_file = syn_inline_mod::parse_and_inline_modules(root_file);

    Ok(r#"
    {
    "name": "grammar",
    "rules": {
        "source_file": {
            "type": "ALIAS",
            "named": false,
            "value": "Expression",
            "content": {
                "type": "SYMBOL",
                "name": "Expression"
            }
        },
        "Expression": {
            "type": "CHOICE",
            "members": [
                {
                    "type": "SYMBOL",
                    "name": "Expression_Number"
                }
            ]
        },
        "Expression_Number": {
            "type": "SEQ",
            "members": [
                {
                    "type": "SYMBOL",
                    "name": "Expression_Number_0"
                }
            ]
        },
        "Expression_Number_0": {
            "type": "PATTERN",
            "value": "\\d+"
        }
    }
    }
    "#
    .to_string())
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        let result = 2 + 2;
        assert_eq!(result, 4);
    }
}
