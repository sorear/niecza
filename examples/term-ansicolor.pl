eval("use Term::ANSIColor;",:lang<perl5>);
my &color := eval('\&color',:lang<perl5>);
say
    color("bold blue"), "Hello ",
    color("green"), "Colorful ",
    color("cyan"),"World",
    color("reset");

