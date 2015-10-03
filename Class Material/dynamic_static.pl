# Global variable
$string = "Hello, World! (Global)";



sub PrintHelloDynamic{
   # the keyword local makes the scoping of $string dynamic.
   local $string;
   $string = "Hello, World! (Dynamic)";
   PrintMe();
   print "Inside the function PrintHelloDynamic. The string value is $string\n";
}
sub PrintMe{
   print "Inside the function PrintMe. The string value is: $string\n";
}

sub PrintHelloLexical{
   # the keyword my makes the scoping of $string lexical or static
   my $string;
   $string = "Hello, World! (Lexical or static)";
   PrintMe();
   print "Inside the function PrintHelloLexical. The string value is $string\n";
}

print "No functions called yet. The string value is $string.\n\n";

print "Call PrintHelloDynamic.\n";
PrintHelloDynamic(); # calls dynamic function.
print "Done with PrintHelloDynamic. \n\nThe string value is $string\n\n\n";

print "Call PrintHelloLexical.\n";
PrintHelloLexical(); # calls lexical or static function
print "Done with PrintHelloLexical. \n\nThe string value is $string\n";




