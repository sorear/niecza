use 5.010;
use utf8;
use warnings;
use strict;

package CLRTypes;

# Beta will do this using reflection
my %typedata = (
    IP6 =>
        { Isa          => [m => 'Boolean'],
          Does         => [m => 'Boolean'],
          GetTypeName  => [m => 'String'],
          GetTypeObject=> [m => 'IP6'],
          GetMO        => [m => 'DynMetaObject'],
          IsDefined    => [m => 'Boolean'],
          HOW          => [c => 'IP6'] },
    DynObject =>
        { klass        => [f => 'DynMetaObject'],
          GetSlot      => [m => 'object'],
          SetSlot      => [m => 'Void'],
          slots        => [f => 'Dictionary<string,Object>'] },

    DynMetaObject =>
        { Complete     => [m => 'Void'],
          HasMRO       => [m => 'Boolean'],
          AddMultiRegex=> [m => 'Void'],
          AddMethod    => [m => 'Void'],
          AddSuperclass=> [m => 'Void'],
          AddAttribute => [m => 'Void'],
          typeObject   => [f => 'IP6'],
          how          => [f => 'IP6'],
          name         => [f => 'String'] },

    'Double' =>
        { ToString     => [m => 'String'] },
    'Variable' =>
        { islist       => [f => 'Boolean'],
          Fetch        => [m => 'IP6' ] },
    'BValue' =>
        { v            => [f => 'Variable' ] },
    'VivClosure' =>
        { v            => [f => 'IP6'] },
    'String' =>
        { Length       => [f => 'Int32'],
          Substring    => [m => 'String'] },
    'System.Text.StringBuilder' =>
        { Append       => [m => 'Void'],
          ToString     => [m => 'String'] },
    'Frame' =>
        { pos          => [f => 'Variable[]'],
          rx           => [f => 'RxFrame'],
          caller       => [f => 'Frame'],
          outer        => [f => 'Frame'],
          proto        => [f => 'Frame'],
          lex          => [f => 'Dictionary<string,object>'],
          lexn         => [f => 'object[]'],
          ExecutingLine=> [m => 'Int32'],
          ExecutingFile=> [m => 'String'],
          ExtractNamed => [m => 'Variable'],
          LexicalFind  => [m => 'Variable'] },
    'RxFrame' =>
        { Exact        => [m => 'Boolean'],
          Exact1       => [m => 'Boolean'],
          GetClass     => [m => 'DynMetaObject'],
          IncQuant     => [m => 'Void'],
          GetQuant     => [m => 'Int32'],
          OpenQuant    => [m => 'Void'],
          CloseQuant   => [m => 'Int32'],
          CommitGroup  => [m => 'Void'],
          CommitRule   => [m => 'Void'],
          CommitAll    => [m => 'Void'],
          PushCutGroup => [m => 'Void'],
          PopCutGroup  => [m => 'Void'],
          GetCursorList=> [m => 'Variable'],
          SetCursorList=> [m => 'Void'],
          LTMPushAlts  => [m => 'Void'],
          PushCapture  => [m => 'Void'],
          MakeCursor   => [m => 'Cursor'],
          MakeMatch    => [m => 'Cursor'],
          SetPos       => [m => 'Void'],
          Backtrack    => [c => 'Void'],
          End          => [c => 'Void'] },
    'Cursor' =>
        { At           => [m => 'Cursor'],
          pos          => [f => 'Int32'],
          from         => [f => 'Int32'],
          GetKey       => [m => 'Variable'],
          backing      => [f => 'String'],
          SimpleWS     => [m => 'Variable'] },
    'Lexer' =>
        { Run          => [m => 'Int32[]'] },
    'VarDeque' =>
        { Push         => [m => 'Void'],
          Unshift      => [m => 'Void'],
          Item         => [i => 'Variable'],
          UnshiftN     => [m => 'Void'],
          Pop          => [m => 'Variable'],
          Shift        => [m => 'Variable'],
          Count        => [m => 'Int32'] },

    'System.IO.File.ReadAllText' => [m => 'System.String'],

    'Lexer.RunProtoregex'  => [m => 'IP6[]'],
    'Lexer.GetLexer'       => [m => 'Lexer'],
    'Kernel.SearchForHandler' => [c => 'Variable'],
    'Kernel.Die'           => [c => 'Variable'],
    'Kernel.CoTake'        => [c => 'Variable'],
    'Kernel.Take'          => [c => 'Variable'],
    'Kernel.GatherHelper'  => [c => 'Frame'],
    'Kernel.ContextHelper' => [m => 'Variable'],
    'Kernel.StrP'          => [f => 'IP6'],
    'Kernel.CallFrameMO'   => [f => 'DynMetaObject'],
    'Kernel.Process'       => [f => 'Variable'],
    'Kernel.Global'        => [f => 'Variable'],
    'Kernel.PackageLookup' => [m => 'BValue'],
    'Kernel.SlurpyHelper'  => [m => 'VarDeque'],
    'Kernel.NewBoundVar'   => [c => 'Variable'],
    'Kernel.Assign'        => [c => 'Void'],
    'Kernel.DefaultNew'    => [m => 'Variable'],
    'Kernel.NewROScalar'   => [m => 'Variable'],
    'Kernel.NewRWScalar'   => [m => 'Variable'],
    'Kernel.NewRWListVar'  => [m => 'Variable'],
    'Console.WriteLine'    => [m => 'Void'],
    'Console.Error.WriteLine'    => [m => 'Void'],
    'System.Environment.Exit'     => [m => 'Void'],
    'String.Concat'        => [m => 'String'],
    'Kernel.AnyP'          => [f => 'IP6'],
    'Kernel.ArrayP'        => [f => 'IP6'],
    'Kernel.HashP'         => [f => 'IP6'],
    'Kernel.StashP',       => [f => 'IP6'],
    'Kernel.SubMO'         => [f => 'DynMetaObject'],
    'Kernel.ScalarMO'      => [f => 'DynMetaObject'],
    'Kernel.StashMO'       => [f => 'DynMetaObject'],
    'Kernel.MainlineContinuation' => [f => 'DynBlockDelegate'],
    'Kernel.MakeSub'       => [m => 'IP6'],
    'Kernel.BoxAny'        => [m => 'Variable'],
    'Kernel.UnboxAny'      => [m => 'object'],
);

my %tmap = (
    'str'       => 'System.String',
    'var'       => 'Niecza.Variable',
    'obj'       => 'Niecza.IP6',
    'int'       => 'System.Int32',
    'num'       => 'System.Double',
);

sub _generic_infer {
    /Dictionary<(.*),(.*)>/ && return {
        ContainsKey         => [ m => 'Boolean' ],
        Item                => [ i => $2 ],
    };
    /List<(.*)>/ && return {
        Add                 => [m => 'Void'],
        Insert              => [m => 'Void'],
        RemoveAt            => [m => 'Void'],
        Count               => [f => 'Int32'],
        Item                => [i => $1],
    };
    /(.*)\[\]/ && return {
        Length              => [f => 'Int32'],
        Item                => [i => $1],
    };
}

sub strip {
    my ($self, $name) = @_;
    $name =~ s/:.*//;
    $name;
}

sub info {
    my ($self, $types, @path) = @_;

    if (!defined ($path[-1])) {
        Carp::confess("Undefined path in _typedata");
    }

    if ($path[-1] =~ /(.*):(.),(.*)/) {
        return $1, $2, $3;
    }

    for ($path[0]) { $typedata{$_} //= _generic_infer; }

    my $cursor = \%typedata;
    for (@path) { $cursor = $cursor->{$_}; }
    if (!defined $cursor) {
        die "No type data for " . join(":", @path);
    }
    if (index($types, $cursor->[0]) < 0) {
        die "Expected [$types] for " . join(":", @path) . " but got " .
            $cursor->[0];
    }
    if (length($types) > 1) {
        return $path[-1], @$cursor;
    } else {
        return $path[-1], $types, $cursor->[1];
    }
}

1;
