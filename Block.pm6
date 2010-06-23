use Sprixel::Meta::Op;
use Sprixel;
use System;
#| Class of blocks of code, which are transparent to control exceptions.
class Block;

#| Base of the optree executed normally (outside any phasers)
has Op $.do;

#| If this is a clone, who we were cloned from.
has Block $.prototype;

#| Outer scope to interpret lexicals in.
has Sprixel::FrameBase $.outer;

#| While I'd love to make C<< postcircumfix:<( )> >> an ordinary operator,
#| this leads to infinite semantic regress.  Instead, it is a reprmethod, and
#| our representation system treats L<Code> and subclasses specially; when
#| invoked, they pass control to $.clr-delegate, which must be a
#| Sprixel.CallableDelegate object.  If not defined, the codegen method will be
#| called.
has #`( Sprixel::CallableDelegate ) $.clr-delegate;

# This will need to be much more subtle after the optimizer and BEGIN time go
# in, in particular for specialization purposes.
method codegen() {
    if ($.prototype) {
        return $.prototype.codegen;
    }

    # TODO all functions need to take an IPerl6Object for parameter parcel /
    # return parcel
    my $body = System::Reflection::DynamicMethod.new("__ANON__",
        System::Void, [ Sprixel::DynamicFrame, System::Int32 ], Block);

    $body.DefineParameter( 0, System::Reflection::ParameterAttributes.In,
        "caller" );
    $body.DefineParameter( 1, System::Reflection::ParameterAttributes.In,
        "resumepoint" );

    my $*IL = $body.GetILGenerator;
    my %*CONTLABELS;

    $.do.compile;

    # load current frame
    $*IL.Emit(System::Reflection::Emit::OpCodes.Ldarg_0);
    # load caller
    $*IL.Emit(System::Reflection::Emit::OpCodes.Ldfld,
        Sprixel::FrameBase.GetField("caller"));
    # load current frame
    $*IL.Emit(System::Reflection::Emit::OpCodes.Ldarg_0);
    # load resumepoint
    $*IL.Emit(System::Reflection::Emit::OpCodes.Ldfld,
        Sprixel::FrameBase.GetField("resumePoint"));
    $*IL.Emit(System::Reflection::Emit::OpCodes.Tailcall);
    $*IL.Emit(System::Reflection::Emit::OpCodes.Callvirt,
        Sprixel::FrameBase.GetMethod("Continue"));
    $*IL.Emit(System::Reflection::Emit::OpCodes.Ret);

    my $bd = $body.CreateDelegate(Sprixel::DynBlockDelegate);

    my $starter = System::Reflection::DynamicMethod.new("__ANON__",
        System::Void, [ Sprixel::FrameBase, Sprixel::FrameBase, System::Int32 ],
        Block);

    $starter.DefineParameter( 0, System::Reflection::ParameterAttributes.In,
        "outer" );
    $starter.DefineParameter( 1, System::Reflection::ParameterAttributes.In,
        "caller" );
    $starter.DefineParameter( 2, System::Reflection::ParameterAttributes.In,
        "returnPoint" );

    my $sil = $starter.GetILGenerator;

    !!!;

    return $starter;
}

#| Called internally; see L<#$.clr-delegate>
method create-delegate() {
    $.code //= self.codegen;

    $.code.CreateDelegate(Sprixel::CallableDelegate, $.outer);
}
