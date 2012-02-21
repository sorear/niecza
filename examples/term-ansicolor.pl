use Term::ANSIColor:from<perl5>;
say
    color("bold blue"), "Hello ",
    color("green"), "Colorful ",
    color("cyan"),"World",
    color("reset");

