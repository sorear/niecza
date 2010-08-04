s/module CORE/module SAFE/;

if (/#\?unsafe (.*)/) {
    print "die \"$1 not allowed in safe mode\";\n";
}

if (/#\?unsafe/ .. /#\?end unsafe/) {
    $_ = '';
}
