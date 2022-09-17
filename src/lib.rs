pub mod asm;
pub mod lir;
pub mod parse;
pub mod targets;
pub mod vm;

/// The value of the NULL pointer constant.
///
/// I've chosen to use the smallest value that can be expressed by an 8-bit signed integer.
/// This is because I want to make sure that this works with 8-bit machines as well.
/// The value of this constant might change in the future though.
pub const NULL: isize = i8::MIN as isize;


/// The UNICODE character art for the logo of the language.
pub const LOGO: &str = "\n ▄▄▄       ▄████▄   ██▓▓█████▄ 
▒████▄    ▒██▀ ▀█  ▓██▒▒██▀ ██▌
▒██  ▀█▄  ▒▓█    ▄ ▒██▒░██   █▌
░██▄▄▄▄██ ▒▓▓▄ ▄██▒░██░░▓█▄   ▌
 ▓█   ▓██▒▒ ▓███▀ ░░██░░▒████▓ 
 ▒▒   ▓▒█░░ ░▒ ▒  ░░▓   ▒▒▓  ▒ 
  ▒   ▒▒ ░  ░  ▒    ▒ ░ ░ ▒  ▒ 
  ░   ▒   ░         ▒ ░ ░ ░  ░ 
      ░  ░░ ░       ░     ░    
          ░             ░      ";

/// The UNICODE character art for the logo of the language, using ANSI escape codes for color.
pub const LOGO_WITH_COLOR: &str = "\n \x1b[32m▄▄▄       ▄████▄   ██\x1b[35m▓▓\x1b[32m█████▄ 
\x1b[35m▒\x1b[32m████▄    \x1b[35m▒\x1b[32m██▀ ▀█  \x1b[35m▓\x1b[32m██\x1b[35m▒▒\x1b[32m██▀ ██▌
\x1b[35m▒\x1b[32m██  \x1b[32m▀█▄  \x1b[35m▒\x1b[32m▓\x1b[32m█    ▄ \x1b[35m▒\x1b[32m██\x1b[35m▒░\x1b[32m██   █▌
\x1b[35m░\x1b[32m██▄▄▄▄██ \x1b[35m▒\x1b[32m▓▓▄ ▄██\x1b[35m▒░\x1b[32m██\x1b[35m░░\x1b[32m▓█▄   ▌
 ▓\x1b[32m█   ▓\x1b[32m██\x1b[35m▒▒ ▓\x1b[32m███▀ \x1b[35m░░\x1b[32m██\x1b[35m░░▒\x1b[32m████▓\x1b[35m 
 \x1b[35m▒▒   ▓▒\x1b[32m█\x1b[35m░░ ░\x1b[35m▒ ▒  \x1b[35m░░\x1b[35m▓   ▒▒▓  \x1b[35m▒
  ▒   ▒▒ ░  ░  ▒    ▒ ░ ░ ▒  ▒ 
  ░   ▒   ░         ▒ ░ ░ ░  ░ 
      ░  ░░ ░       ░     ░    
          ░             ░      \x1b[0m";