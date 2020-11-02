use crate::calc::*;
use crate::types::*;

use nom::{
    error::ParseError,
    IResult,
    branch::alt,
    bytes::complete::{tag, take_while1, escaped_transform},
    character::complete::{none_of,char, digit1,space0},
    combinator::map,
    multi::separated_list0,
    number::complete::{double},
    sequence::{delimited, tuple},
  };

pub fn parse_expr(input: &str) -> IResult<&str, Expr> {
    alt((parse_range,parse_ref,parse_func,parse_value))(space0(input)?.0)
}

fn parse_ref(input: &str) -> IResult<&str, Expr> {
    map(parse_id, Expr::Reference)(input)
}

pub fn parse_id(input: &str) -> IResult<&str, CellID> {
    let mut row = 0;
    let mut col = 0;

    let (input, cols) = take_while1(|c: char| c.is_ascii_alphabetic())(input)?;
    for c in cols.to_uppercase().chars() {
        col= col*26 + (c as u8 - b'A' + 1) as usize;
    }
    let (input, rows) = take_while1(|c:char| c.is_ascii_digit())(input)?;
    for c in rows.chars() {
        let d = c.to_digit(10).unwrap();
        row=row*10 +d as usize;
    }
    Ok((input,CellID{col:col-1, row:row-1}))
}

fn parse_range(input: &str) -> IResult<&str, Expr> {
    let (input,(from,_,to)) = tuple((parse_id, tag(":"), parse_id))(input)?;
    Ok((input,Expr::Range{from,to}))
}

fn parse_func(input: &str) -> IResult<&str, Expr> {
    let name_parser = take_while1(|c: char| c.is_alphanumeric());
    let args_parser = separated_list0(spaced(","), parse_expr);
    let (input, (name,args)) = tuple((name_parser,delimited(spaced("("), args_parser, spaced(")"))))(input)?;

    Ok((input,Expr::Function{name:name.to_string().to_uppercase(),args}))
}

fn spaced<'a, Error: ParseError<&'a str>>(txt: &'a str) -> impl Fn(&'a str) -> IResult<&'a str,&'a str, Error> {
    //tag(txt)
    move |input| {
        let (input,_)=space0(input)?;
        let (input,t)=tag(txt)(input)?;
        let (input,_)=space0(input)?;
        Ok((input,t))
    }
}

fn parse_value(input: &str) -> IResult<&str, Expr> {
    alt((parse_true,parse_false,parse_int, parse_float,parse_string))(input)
}

fn parse_true(input: &str) -> IResult<&str, Expr> {
    let (input,_)=tag("true")(input)?;
    Ok((input,Expr::Value(CellValue::Boolean(true))))
}

fn parse_false(input: &str) -> IResult<&str, Expr> {
    let (input,_)=tag("false")(input)?;
    Ok((input,Expr::Value(CellValue::Boolean(true))))
}

fn parse_int(input: &str) -> IResult<&str, Expr> {
    let (input,str) = digit1(input)?;
    Ok((input,Expr::Value(CellValue::Integer(str.parse().unwrap()))))
}

fn parse_float(input: &str) -> IResult<&str, Expr> {
    let (input, dbl)=double(input)?;
    Ok((input,Expr::Value(CellValue::Float(dbl))))
}

fn parse_string(input: &str) -> IResult<&str, Expr> {
    //none_of(r#""\"#)
    let string_parser = escaped_transform(none_of(r#""\"#), '\\',     parse_string_control );
    let delim_parser = delimited(char('"'),string_parser,char('"'));
    map(delim_parser,|s: String| Expr::Value(CellValue::Text(s)))(input)
}

fn parse_string_control(input: &str) -> IResult<&str, &str> {
    alt((tag("\\"), tag("\""), parse_newline))(input)
}

fn parse_newline(input: &str) -> IResult<&str, &str> {
    let (input,_)=tag("n")(input)?;
    Ok((input,"\n"))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_ref(){
        assert_eq!(Ok(("",Expr::Reference(CellID{row:0,col:0}))), parse_ref("A1"));
        assert_eq!(Ok(("",Expr::Reference(CellID{row:0,col:1}))), parse_ref("B1"));
        assert_eq!(Ok(("",Expr::Reference(CellID{row:0,col:25}))), parse_ref("Z1"));
        assert_eq!(Ok(("",Expr::Reference(CellID{row:0,col:26}))), parse_ref("AA1"));
        assert_eq!(Ok(("",Expr::Reference(CellID{row:0,col:51}))), parse_ref("AZ1"));
        assert_eq!(Ok(("",Expr::Reference(CellID{row:9,col:0}))), parse_ref("A10"));
        assert_eq!(Ok(("",Expr::Reference(CellID{row:99,col:1}))), parse_ref("B100"));
        assert_eq!(Ok(("",Expr::Reference(CellID{row:24,col:25}))), parse_ref("Z25"));
        assert_eq!(Ok(("",Expr::Reference(CellID{row:87,col:26}))), parse_ref("AA88"));
        assert_eq!(Ok(("",Expr::Reference(CellID{row:344,col:51}))), parse_ref("AZ345"));
        
        assert!(parse_ref("hello").is_err());
        assert!(parse_ref("123").is_err());
        assert!(parse_ref("A-1").is_err());
    }

    #[test]
    fn test_parse_range(){
        assert_eq!(Ok(("",Expr::Range{from:CellID{row:0,col:0},to:CellID{row:2,col:1}})), parse_range("A1:B3"));
        assert!(parse_range("A1").is_err());
        assert!(parse_range("A1:").is_err());
        assert!(parse_range(":A1").is_err());
        assert!(parse_range("hello:A1").is_err());
    }

    #[test]
    fn test_parse_function(){
        assert_eq!(Ok(("",Expr::Function{name:"SUM".to_string(),args:vec![Expr::Range{from:CellID{row:0,col:0},to:CellID{row:2,col:1}}]})), parse_func("SUM(A1:B3)"));
        assert_eq!(Ok(("",Expr::Function{name:"SUM".to_string(),args:vec![Expr::Reference(CellID{row:0,col:0}),Expr::Reference(CellID{row:2,col:1})]})), parse_func("sum(A1,B3)"));
        assert_eq!(Ok(("",Expr::Function{name:"SUM".to_string(),args:vec![Expr::Reference(CellID{row:0,col:0}),Expr::Reference(CellID{row:2,col:1})]})), parse_func("sum ( A1,  B3 ) "));
        
        assert_eq!(Ok(("",Expr::Function{name:"SUM".to_string(),args:vec![]})), parse_func("SUM()"));

        assert!(parse_func("SUM").is_err());
        assert!(parse_func("SUM(").is_err());
        assert!(parse_func("SUM)").is_err());
        assert!(parse_func("SUM(,)").is_err());
    }

    #[test]
    fn test_parse_string(){
       
        assert_eq!(Ok(("",Expr::Value(CellValue::Text("abc".to_string())))),parse_string(r#""abc""#));
        assert_eq!(Ok(("",Expr::Value(CellValue::Text("ab\"cd".to_string())))),parse_string(r#""ab\"cd""#));
        assert_eq!(Ok(("",Expr::Value(CellValue::Text("ab\ncd".to_string())))),parse_string(r#""ab\ncd""#));
    }

    #[test]
    fn test_parse_expr(){
        assert_eq!(Ok(("",Expr::Reference(CellID{row:0,col:0}))), parse_expr("A1"));
        assert_eq!(Ok(("",Expr::Range{from:CellID{row:0,col:0},to:CellID{row:2,col:1}})), parse_expr("A1:B3"));
        assert_eq!(Ok(("",Expr::Function{name:"SUM".to_string(),args:vec![Expr::Range{from:CellID{row:0,col:0},to:CellID{row:2,col:1}}]})), parse_expr("  SUM( A1:B3 )"));
        assert_eq!(Ok(("",Expr::Value(CellValue::Text("ab\ncd".to_string())))),parse_expr(r#""ab\ncd""#));
    }
}