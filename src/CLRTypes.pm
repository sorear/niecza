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
          GetSlot      => [m => 'object'],
          SetSlot      => [m => 'Void'],
          GetTypeName  => [m => 'String'],
          GetTypeObject=> [m => 'IP6'],
          IsDefined    => [m => 'Boolean'],
          mo           => [f => 'DynMetaObject'],
          HOW          => [c => 'IP6'] },
    DynObject =>
        { slots        => [f => 'Dictionary<string,Object>'] },

    DynMetaObject =>
        { FillClass    => [m => 'Void'],
          FillRole     => [m => 'Void'],
          FillParametricRole => [m => 'Void'],
          HasMRO       => [m => 'Boolean'],
          AddMultiRegex=> [m => 'Void'],
          AddMethod    => [m => 'Void'],
          AddPrivateMethod => [m => 'Void'],
          GetPrivateMethod => [m => 'IP6'],
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
          SetClass     => [m => 'Void'],
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
          FinalEnd     => [c => 'Void'],
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
    'Variable.None'        => [f => 'Variable[]'],
    'Kernel.SearchForHandler' => [c => 'Variable'],
    'Kernel.Die'           => [c => 'Variable'],
    'Kernel.BindFail'      => [c => 'Variable'],
    'Kernel.CheckArgEnd'   => [c => 'Void'],
    'Kernel.CoTake'        => [c => 'Variable'],
    'Kernel.GetFirst'      => [c => 'Variable'],
    'Kernel.Take'          => [c => 'Variable'],
    'Kernel.GatherHelper'  => [c => 'Frame'],
    'Kernel.ContextHelper' => [m => 'Variable'],
    'Kernel.InstantiateRole' => [c => 'Variable'],
    'Kernel.RoleApply'     => [m => 'DynMetaObject'],
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
    'Kernel.AnyMO'         => [f => 'DynMetaObject'],
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

%{ $typedata{ $_->[0] } } = (%{ $typedata{ $_->[0] } },
    %{ $typedata{ $_->[1] } }) for ( ['DynObject', 'IP6'] );

my %tmap = (
    # a simple string, like string or $S32.  May support nulls
    'str'       => 'String',
    'strbuf'    => 'System.Text.StringBuilder',
    # a reference to a container + usage flags
    'var'       => 'Variable',
    # a reference to a single Perl 6 object, decontainerized
    'obj'       => 'IP6',
    'int'       => 'Int32',
    'num'       => 'Double',
    'bool'      => 'Boolean',
    'varhash'   => 'Dictionary<string,Variable>',
    'fvarlist'  => 'Variable[]',
    'vvarlist'  => 'VarDeque',
    'stab'      => 'DynMetaObject',
    # portable IO is hard, we let CgOp fake it for now?
    'treader'   => 'System.IO.TextReader',
    'twriter'   => 'System.IO.TextWriter',
    # this is a fair leak as it reveals that Frame is its own repr.
    'frame'     => 'Frame',
    'cursor'    => 'Cursor',
    'lad'       => 'LAD',
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
    for (@path) { $cursor = $cursor && $cursor->{$_}; }
    if (!$cursor) {
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

sub mapt {
    my ($self, $t) = @_;
    if ($tmap{$t}) { return $tmap{$t} }
    if (substr($t,0,4) eq 'clr:') {
        return substr($t,4);
    }
    warn "Invalid type form $t\n";
    return $t;
}

1;
