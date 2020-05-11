lalrpop_mod!(pub calculator1); // synthesized by LALRPOP

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;

    #[test]
    fn calculator1() {
        assert!(calculator1::TermParser::new().parse("22").is_ok());
        assert!(calculator1::TermParser::new().parse("(22)").is_ok());
        assert!(calculator1::TermParser::new().parse("((((22))))").is_ok());
        assert!(calculator1::TermParser::new().parse("((22)").is_err());
    }
}
