using Niecza;
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;


public partial class Builtins {

    public static Variable sprintf(Variable[] args) {
        return args[0];
    }
}