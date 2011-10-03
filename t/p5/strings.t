use Test;
is(eval("'Yet Another Perl Hacker'",:lang<perl5>),"Yet Another Perl Hacker");
is(eval('"Yet Ano\0ther P\0erl Hacker"',:lang<perl5>),"Yet Ano\0ther P\0erl Hacker","Null Bytes in the middle of a converted string");
is(eval('"ąęóśćż"',:lang<perl5>),"ąęóśćż","utf8 in literals");
done;
