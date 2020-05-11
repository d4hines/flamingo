lalrpop_mod!(pub calculator); // synthesized by LALRPOP
lalrpop_mod!(pub parse);

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::*;
    use std::fs;

    #[test]
    fn calculator5() {
        let expr = calculator::ExprsParser::new().parse("").unwrap();
        assert_eq!(&format!("{:?}", expr), "[]");

        let expr = calculator::ExprsParser::new()
            .parse("22 * 44 + 66")
            .unwrap();
        assert_eq!(&format!("{:?}", expr), "[((22 * 44) + 66)]");

        let expr = calculator::ExprsParser::new()
            .parse("22 * 44 + 66,")
            .unwrap();
        assert_eq!(&format!("{:?}", expr), "[((22 * 44) + 66)]");

        let expr = calculator::ExprsParser::new()
            .parse("22 * 44 + 66, 13*3")
            .unwrap();
        assert_eq!(&format!("{:?}", expr), "[((22 * 44) + 66), (13 * 3)]");

        let expr = calculator::ExprsParser::new()
            .parse("22 * 44 + 66, 13*3,")
            .unwrap();
        assert_eq!(&format!("{:?}", expr), "[((22 * 44) + 66), (13 * 3)]");
    }

    #[test]
    fn foo() {
        let module =
            fs::read_to_string("./test/logic.alm").expect("Failed to read ./test/logic.alm");
        let parsed = parse::ALMModuleParser::new()
            .parse(module.as_str())
            .unwrap();
        assert_eq!(
            parsed,
            ALMModule {
                enums: vec![Enum {
                    name: "Directions".to_string(),
                    terms: vec![
                        "DTop".to_string(),
                        "DLeft".to_string(),
                        "DBottom".to_string(),
                        "DRight".to_string(),
                    ],
                }],
                sorts: vec![Sort {
                    name: "Rectangles".to_string(),
                    parent_sorts: vec!["Universe".to_string()],
                    attributes: Some(vec![
                        FunctionDeclaration {
                            name: "Width".to_string(),
                            params: None,
                            ret: "Integers".to_string(),
                        },
                        FunctionDeclaration {
                            name: "Height".to_string(),
                            params: None,
                            ret: "Integers".to_string(),
                        }
                    ])
                },],
                statics: vec![
                    FunctionDeclaration {
                        name: "Opposite_Direction".to_string(),
                        params: Some(vec!["Directions".to_string()]),
                        ret: "Directions".to_string(),
                    },
                    FunctionDeclaration {
                        name: "Snapping_Threshold".to_string(),
                        params: None,
                        ret: "Integers".to_string(),
                    },
                ],
                fluents: Fluents {
                    basic: vec![
                        FunctionDeclaration {
                            name: "Grouped_With".to_string(),
                            params: Some(vec!["Rectangles".to_string(), "Rectangles".to_string()]),
                            ret: "Booleans".to_string(),
                        },
                        FunctionDeclaration {
                            name: "Moving".to_string(),
                            params: Some(vec!["Rectangles".to_string()]),
                            ret: "Booleans".to_string(),
                        },
                    ],
                    defined: vec![],
                },
                axioms: vec![],
            }
        );
    }
}
