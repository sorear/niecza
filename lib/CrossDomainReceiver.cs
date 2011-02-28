// This is basically a workaround for ugliness in the CLR APIs for
// cross-domain communication ...

using System;
namespace Niecza {
    public abstract class CrossDomainReceiver : MarshalByRefObject {
        public abstract string[] Call(AppDomain from, string[] args);
    }
}
