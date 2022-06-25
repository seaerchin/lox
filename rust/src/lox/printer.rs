use crate::expr::{Expr, Op, RawLiteral, UnaryOp};

// NOTE: This doesn't follow the book - the book implements this using the
// visitor pattern, but we just recurse + pattern match ._.
// Given an expr, we want to add spaces between each item of the expr
// we should also have parens before/after each expr
fn paren(name: &str, expr: Expr) -> String {
    let mut cur_rep = format!("({name}");
    match expr {
        Expr::Literal(raw) => {
            let lit = raw.to_string();
            cur_rep = cur_rep + &lit;
        }
        Expr::Grouping(g) => cur_rep += &paren("group ", *g),
        Expr::Unary(u, expr) => {
            let unary_rep = u.to_string() + " ";
            cur_rep += &(unary_rep + &paren("", *expr))
        }
        Expr::Binary(first, op, second) => {
            let first_rep = paren("", *first);
            let second_rep = paren("", *second);
            let op_rep = op.to_string();

            let consolidated = op_rep + " " + &first_rep + " " + &second_rep;

            cur_rep += &consolidated;
        }
    }

    return format!("{cur_rep})");
}

fn indented(expr: Expr) -> String {
    // TODO: implement pretty printing using indentation
    todo!()
}
