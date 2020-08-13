mod lexer;
mod parser;
fn main() {
    let input = "
  let add = fn(x, y) {
      let a = x + y - z < w * a / b != k == d;
  };
  if (5 < 10) {
      return true;
  } else {
      return false;
  }";
    lexer::start_to_tokenize(input);
}
