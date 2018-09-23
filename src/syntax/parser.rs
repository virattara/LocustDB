use sqlparser::sqlparser::*;
use sqlparser::sqlast::*;
use engine::query::Query;
use syntax::expression::*;
use engine::aggregator::*;
use ingest::raw_val::RawVal;
use syntax::limit::*;

pub fn parse_query(query: &str) -> Result<Query, ParserError> {
    let ast = Parser::parse_sql(query.to_string())?;

    let mut select = Vec::<Expr>::new();
    let mut aggregate = Vec::<(Aggregator, Expr)>::new();
    let mut table = String::new();
    let mut filter = Expr::Const(RawVal::Int(1));
    let mut order_by_str = None;
    let mut order_desc = false;
    let mut limit_clause = LimitClause { limit: 100, offset: 0 };
    let order_by_index = None;

    if let ASTNode::SQLSelect 
        {projection, relation, selection, order_by, limit, .. } = ast.to_owned() 
    {
        for elem in projection {
            match elem.to_owned() {
                ASTNode::SQLIdentifier(expr) => 
                    select.push(Expr::ColName(expr)),
                ASTNode::SQLLiteralLong(constant) => 
                    select.push(Expr::Const(RawVal::Int(constant))),
                ASTNode::SQLWildcard => 
                    select.push(Expr::ColName('*'.to_string())),
                ASTNode::SQLFunction{id, args} => {
                    let aggregator = match id.to_uppercase().as_ref() {
                        "COUNT" => Aggregator::Count,
                        "SUM" => Aggregator::Sum,
                        _ => panic!("Unknown aggregate fn"),
                    };

                    let expr = match args[0].to_owned() {
                        ASTNode::SQLLiteralLong(literal) => 
                            Expr::Const(RawVal::Int(literal)),
                        ASTNode::SQLIdentifier(identifier) => 
                            Expr::ColName(identifier),
                        ASTNode::SQLBinaryExpr{left, op, right} => 
                            generate_expression(ASTNode::SQLBinaryExpr{left, op, right}),
                        _ => Expr::Const(RawVal::Int(1))
                    };

                    aggregate.push((aggregator, expr));
                },
                _ => eprintln!("Unknown Pattern {:?}", elem),
            }
        }

        table = match relation {
            Some(sql_identifier) => {
                if let ASTNode::SQLIdentifier(tb) = *sql_identifier {
                   tb
                } else {
                    return Err(ParserError::ParserError(format!("Failed to parse: No table selected.")));
                }
            },
            None => {
                return Err(ParserError::ParserError(format!("Failed to parse: No table selected.")));
            }
        };

        filter = match selection {
            Some(binary_expr) => generate_expression(*binary_expr),
            None => Expr::Const(RawVal::Int(1))
        };

        match order_by {
            Some(sql_order_by) => if let ASTNode::SQLOrderBy{expr, asc} =  sql_order_by[0].to_owned() {
                if let ASTNode::SQLIdentifier(identifier) = *expr {
                    order_by_str = Some(identifier);
                }
                order_desc = asc;
            },
            None => (),
        };

        match limit {
            Some(sql_limit) => if let ASTNode::SQLLiteralLong(literal) = *sql_limit {
                limit_clause = LimitClause { limit: literal as u64, offset: 0 };
            },
            None => (),
        };
    }

    let result = Query {
        select,
        table,
        filter,
        aggregate,
        order_by: order_by_str,
        order_desc,
        limit: limit_clause,
        order_by_index,
    };

    Ok(result)
}

fn generate_expression(binary_expr: ASTNode) -> Expr {
    if let ASTNode::SQLBinaryExpr{left, op, right} = binary_expr {
        let operator = match op {
            SQLOperator::And => Func2Type::And,
            SQLOperator::Plus => Func2Type::Add,
            SQLOperator::Minus => Func2Type::Subtract,
            SQLOperator::Multiply => Func2Type::Multiply,
            SQLOperator::Divide => Func2Type::Divide,
            SQLOperator::Gt => Func2Type::GT,
            SQLOperator::Lt => Func2Type::LT,
            SQLOperator::Eq => Func2Type::Equals,
            SQLOperator::NotEq => Func2Type::NotEquals,
            SQLOperator::Or => Func2Type::Or,
            _ => {
                eprintln!("Unknown operator {:?}", op);
                Func2Type::Equals
            },
        };

        let lhs = match *left {
            ASTNode::SQLLiteralLong(literal) => Box::new(Expr::Const(RawVal::Int(literal))),
            ASTNode::SQLIdentifier(identifier) => Box::new(Expr::ColName(identifier)),
            _ => Box::new(Expr::Const(RawVal::Int(1)))
        };

        let rhs = match *right {
            ASTNode::SQLLiteralLong(literal) => Box::new(Expr::Const(RawVal::Int(literal))),
            ASTNode::SQLIdentifier(identifier) => Box::new(Expr::ColName(identifier)),
            _ => Box::new(Expr::Const(RawVal::Int(1)))
        };

        Expr::Func2(operator, lhs, rhs)
    } else {
        Expr::Const(RawVal::Int(1))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_select_star() {
        assert_eq!(
            format!("{:?}", parse_query("select * from default;".as_bytes())),
            "Done([], Query { select: [ColName(\"*\")], table: \"default\", filter: Const(Int(1)), aggregate: [], order_by: None, order_desc: false, limit: LimitClause { limit: 100, offset: 0 }, order_by_index: None })");
    }

    #[test]
    fn test_last_hour() {
        assert!(
            format!("{:?}", parse_query("select * from default where $LAST_HOUR;".as_bytes())).starts_with(
                "Done([], Query { select: [ColName(\"*\")], table: \"default\", filter: Func2(GT, ColName(\"timestamp\"), Const(Int(")
        )
    }

    #[test]
    fn test_to_year() {
        assert_eq!(
            format!("{:?}", parse_query("select to_year(ts) from default;".as_bytes())),
            "Done([], Query { select: [Func1(ToYear, ColName(\"ts\"))], table: \"default\", filter: Const(Int(1)), aggregate: [], order_by: None, order_desc: false, limit: LimitClause { limit: 100, offset: 0 }, order_by_index: None })");
    }
}
