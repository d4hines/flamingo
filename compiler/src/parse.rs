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
                    defined: vec![
                        DefinedFluentDeclaration {
                            output: true,
                            declaration: FunctionDeclaration {
                                name: "Side".to_string(),
                                params: Some(vec![
                                    "Rectangles".to_string(),
                                    "Directions".to_string()
                                ]),
                                ret: "Booleans".to_string(),
                            },
                        },
                        DefinedFluentDeclaration {
                            output: false,
                            declaration: FunctionDeclaration {
                                name: "Distance".to_string(),
                                params: Some(vec![
                                    "Rectangles".to_string(),
                                    "Rectangles".to_string(),
                                    "Integers".to_string(),
                                ]),
                                ret: "Booleans".to_string(),
                            },
                        },
                    ],
                },
                axioms: vec![
                    Axiom::StaticAssignment {
                        name: "Snapping_Threshold".to_string(),
                        value: 30,
                    },
                    Axiom::Rule {
                        head: FunctionAssignment {
                            negated: false,
                            name: "Opposite_Direction".to_string(),
                            arguments: vec![Expression::ExpressionVariable("a".to_string())],
                            value: Some(Expression::ExpressionVariable("b".to_string())),
                        },
                        body: vec![RuleClause::ClauseFunctionAssignment(FunctionAssignment {
                            negated: false,
                            name: "Opposite_Direction".to_string(),
                            arguments: vec![Expression::ExpressionVariable("b".to_string())],
                            value: Some(Expression::ExpressionVariable("a".to_string())),
                        })],
                    },
                    Axiom::Fact(FunctionAssignment {
                        negated: false,
                        name: "Opposite_Direction".to_string(),
                        arguments: vec![Expression::ExpressionTerm(Term::TermUpper(
                            "DLeft".to_string(),
                        ))],
                        value: Some(Expression::ExpressionTerm(Term::TermUpper(
                            "DRight".to_string(),
                        ))),
                    }),
                    Axiom::Rule {
                        head: FunctionAssignment {
                            negated: false,
                            name: "Distance".to_string(),
                            arguments: vec![
                                Expression::ExpressionVariable("a".to_string()),
                                Expression::ExpressionVariable("b".to_string()),
                                Expression::ExpressionVariable("min_d".to_string()),
                            ],
                            value: None,
                        },
                        body: vec![
                            RuleClause::ClauseFunctionAssignment(FunctionAssignment {
                                negated: false,
                                name: "Instance".to_string(),
                                arguments: vec![
                                    Expression::ExpressionVariable("a".to_string()),
                                    Expression::ExpressionTerm(Term::TermUpper(
                                        "Rectangles".to_string(),
                                    )),
                                ],
                                value: None,
                            }),
                            RuleClause::ClauseFunctionAssignment(FunctionAssignment {
                                negated: false,
                                name: "Instance".to_string(),
                                arguments: vec![
                                    Expression::ExpressionVariable("b".to_string()),
                                    Expression::ExpressionTerm(Term::TermUpper(
                                        "Rectangles".to_string(),
                                    )),
                                ],
                                value: None,
                            }),
                            RuleClause::ClauseFunctionAssignment(FunctionAssignment {
                                negated: true,
                                name: "Overlaps".to_string(),
                                arguments: vec![
                                    Expression::ExpressionVariable("a".to_string()),
                                    Expression::ExpressionVariable("b".to_string()),
                                ],
                                value: None,
                            }),
                            RuleClause::ClauseFunctionAssignment(FunctionAssignment {
                                negated: false,
                                name: "Opposite_Direction".to_string(),
                                arguments: vec![Expression::ExpressionVariable("dir".to_string())],
                                value: Some(Expression::ExpressionVariable("dir'".to_string())),
                            }),
                            RuleClause::ClauseRawDDLog(
                                "var min_d = Aggregate((a, b), group_min(b))".to_string(),
                            ),
                        ],
                    },
                    Axiom::Rule {
                        head: FunctionAssignment {
                            negated: true,
                            name: "Moving".to_string(),
                            arguments: vec![Expression::ExpressionVariable("other".to_string())],
                            value: None,
                        },
                        body: vec![
                            RuleClause::ClauseFunctionAssignment(FunctionAssignment {
                                negated: false,
                                name: "Instance".to_string(),
                                arguments: vec![
                                    Expression::ExpressionVariable("other".to_string()),
                                    Expression::ExpressionTerm(Term::TermUpper(
                                        "Rectangles".to_string(),
                                    )),
                                ],
                                value: None,
                            }),
                            RuleClause::ClauseFunctionAssignment(FunctionAssignment {
                                negated: true,
                                name: "Grouped_With".to_string(),
                                arguments: vec![
                                    Expression::ExpressionVariable("other".to_string()),
                                    Expression::ExpressionVariable("_".to_string()),
                                ],
                                value: None,
                            }),
                        ],
                    },
                ],
            }
        );
    }
}
