eval("use Term::ANSIColor;",:lang<perl5>);
say
    eval('color "bold blue"',:lang<perl5>),
    "Hello ",
    eval('color "green"',:lang<perl5>),
    "Colorful ",
    eval('color "cyan"',:lang<perl5>),
    "World",
    eval('color "reset"',:lang<perl5>);

