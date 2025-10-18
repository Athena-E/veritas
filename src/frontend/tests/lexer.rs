use crate::common::ast::Token;
use super::common::parse_tokens;

#[test]
fn test_lexer_basic_statement() {
    let tokens = parse_tokens("let x: int = 42;");
    assert_eq!(tokens.len(), 7);
    assert_eq!(tokens[0].0, Token::Let);
    assert_eq!(tokens[1].0, Token::Ident("x"));
    assert_eq!(tokens[2].0, Token::Ctrl(':'));
    assert_eq!(tokens[3].0, Token::Int);
    assert_eq!(tokens[4].0, Token::Op("="));
    assert_eq!(tokens[5].0, Token::Num(42));
    assert_eq!(tokens[6].0, Token::Ctrl(';'));
}

#[test]
fn test_lexer_comparison_operators() {
    let tokens = parse_tokens("x == 5 && y != 3");
    assert_eq!(tokens[1].0, Token::Op("=="));
    assert_eq!(tokens[3].0, Token::Op("&&"));
    assert_eq!(tokens[5].0, Token::Op("!="));
}

#[test]
fn test_lexer_all_comparison_operators() {
    let tokens = parse_tokens("< > <= >= == !=");
    assert_eq!(tokens[0].0, Token::Op("<"));
    assert_eq!(tokens[1].0, Token::Op(">"));
    assert_eq!(tokens[2].0, Token::Op("<="));
    assert_eq!(tokens[3].0, Token::Op(">="));
    assert_eq!(tokens[4].0, Token::Op("=="));
    assert_eq!(tokens[5].0, Token::Op("!="));
}

#[test]
fn test_lexer_arithmetic_operators() {
    let tokens = parse_tokens("+ - * = & | !");
    assert_eq!(tokens[0].0, Token::Op("+"));
    assert_eq!(tokens[1].0, Token::Op("-"));
    assert_eq!(tokens[2].0, Token::Op("*"));
    assert_eq!(tokens[3].0, Token::Op("="));
    assert_eq!(tokens[4].0, Token::Op("&"));
    assert_eq!(tokens[5].0, Token::Op("|"));
    assert_eq!(tokens[6].0, Token::Op("!"));
}

#[test]
fn test_lexer_logical_operators() {
    let tokens = parse_tokens("&& ||");
    assert_eq!(tokens[0].0, Token::Op("&&"));
    assert_eq!(tokens[1].0, Token::Op("||"));
}

#[test]
fn test_lexer_control_characters() {
    let tokens = parse_tokens("( ) { } [ ] ; , :");
    assert_eq!(tokens[0].0, Token::Ctrl('('));
    assert_eq!(tokens[1].0, Token::Ctrl(')'));
    assert_eq!(tokens[2].0, Token::Ctrl('{'));
    assert_eq!(tokens[3].0, Token::Ctrl('}'));
    assert_eq!(tokens[4].0, Token::Ctrl('['));
    assert_eq!(tokens[5].0, Token::Ctrl(']'));
    assert_eq!(tokens[6].0, Token::Ctrl(';'));
    assert_eq!(tokens[7].0, Token::Ctrl(','));
    assert_eq!(tokens[8].0, Token::Ctrl(':'));
}

#[test]
fn test_lexer_keywords() {
    let tokens = parse_tokens("fn let mut if else return int bool true false");
    assert_eq!(tokens[0].0, Token::Fn);
    assert_eq!(tokens[1].0, Token::Let);
    assert_eq!(tokens[2].0, Token::Mut);
    assert_eq!(tokens[3].0, Token::If);
    assert_eq!(tokens[4].0, Token::Else);
    assert_eq!(tokens[5].0, Token::Return);
    assert_eq!(tokens[6].0, Token::Int);
    assert_eq!(tokens[7].0, Token::Bool);
    assert_eq!(tokens[8].0, Token::True);
    assert_eq!(tokens[9].0, Token::False);
}

#[test]
fn test_lexer_identifiers() {
    let tokens = parse_tokens("foo bar_baz my_var");
    assert_eq!(tokens.len(), 3);
    assert_eq!(tokens[0].0, Token::Ident("foo"));
    assert_eq!(tokens[1].0, Token::Ident("bar_baz"));
    assert_eq!(tokens[2].0, Token::Ident("my_var"));
}

#[test]
fn test_lexer_numbers() {
    let tokens = parse_tokens("0 42 123456");
    assert_eq!(tokens[0].0, Token::Num(0));
    assert_eq!(tokens[1].0, Token::Num(42));
    assert_eq!(tokens[2].0, Token::Num(123456));
}

#[test]
fn test_lexer_comments() {
    let tokens = parse_tokens("let x = 42; // this is a comment\nlet y = 10;");
    // Comments should be filtered out
    assert_eq!(tokens[0].0, Token::Let);
    assert_eq!(tokens[1].0, Token::Ident("x"));
    assert_eq!(tokens[2].0, Token::Op("="));
    assert_eq!(tokens[3].0, Token::Num(42));
    assert_eq!(tokens[4].0, Token::Ctrl(';'));
    assert_eq!(tokens[5].0, Token::Let);
    assert_eq!(tokens[6].0, Token::Ident("y"));
}

#[test]
fn test_lexer_whitespace_handling() {
    let tokens = parse_tokens("  let   x  :  int  =  42  ;  ");
    assert_eq!(tokens.len(), 7);
    assert_eq!(tokens[0].0, Token::Let);
}

#[test]
fn test_lexer_mutable_variable() {
    let tokens = parse_tokens("let mut x: int = 5;");
    assert_eq!(tokens[0].0, Token::Let);
    assert_eq!(tokens[1].0, Token::Mut);
    assert_eq!(tokens[2].0, Token::Ident("x"));
}
