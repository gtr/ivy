// Ivy gives the user two ways of defining data: via a struct and via an enum.
// A struct is like a record in other languages: it holds named data called 
// "fields" of different types. An enum is very similar to an enum in Rust: it
// allows for a predefined set of constants.

// Here we define a Person struct which combines three kinds of data: name, 
// age, and zodiac. By convention, the symbol names for structs start with an
// uppercase character.
let Person = struct (name, age, zodiac);

// Here we define a Sign enum which can be one of the twelve zodiac signs. By
// convention, the symbol names for enums start with an uppercase character.
let Sign = enum (
    | Aries         // March 21 – April 19
    | Taurus        // April 20 – May 20
    | Gemini        // May 21 – June 20
    | Cancer        // June 21 – July 22
    | Leo           // July 23 – August 22
    | Virgo         // August 23 – September 22
    | Libra         // September 23 – October 22
    | Scorpio       // October 23 – November 21
    | Sagittarius   // November 22 – December 21
    | Capricorn     // December 22 – January 19
    | Aquarius      // January 20 – February 18
    | Pisces        // February 19 – March 20
);

// Using the Person struct and the Sign struct, we define a symbol ceci 
// representing a Person with name of string "Cecilia", age of integer 24, and
// zodiac of Sign Aries.
let ceci = Person("Cecilia", 24, Sign(Aries));

// We can read the fields from symbol ceci:
// Prints: "Cecilia is 24 years old and is an Aries".
println(ceci@name, " is ", ceci@age, " years old and is an ", ceci@zodiac);

