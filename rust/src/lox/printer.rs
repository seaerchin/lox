use crate::expr::ExprType;

// NOTE: This doesn't follow the book - the book implements this using the
// visitor pattern, but we just recurse + pattern match ._.
// Given an expr, we want to add spaces between each item of the expr
// we should also have parens before/after each expr
fn paren(name: &str, expr: ExprType) -> String {
    let mut cur_rep = format!("({name}");
    match expr {
        ExprType::Literal(raw) => {
            let lit = raw.to_string();
            cur_rep = cur_rep + &lit;
        }
        ExprType::Grouping(g) => cur_rep += &paren("group ", g.expr),
        ExprType::Unary(u, expr) => {
            let unary_rep = u.to_string() + " ";
            cur_rep += &(unary_rep + &paren("", expr.expr))
        }
        ExprType::Binary(first, op, second) => {
            let first_rep = paren("", first.expr);
            let second_rep = paren("", second.expr);
            let op_rep = op.to_string();

            let consolidated = op_rep + " " + &first_rep + " " + &second_rep;

            cur_rep += &consolidated;
        }
        ExprType::Variable(tok) => cur_rep += &tok.literal.to_string(),
    }

    return format!("{cur_rep})");
}

fn indented(expr: ExprType) -> String {
    // TODO: implement pretty printing using indentation
    todo!()
}
