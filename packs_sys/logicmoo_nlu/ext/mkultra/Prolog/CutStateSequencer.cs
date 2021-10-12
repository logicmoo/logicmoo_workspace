using System;
using System.Collections;
using System.Collections.Generic;

namespace Prolog
{
    /// <summary>
    /// A placeholder enumerator that primitives can return when they just want to succeed or fail
    /// without the overhead of the state machine craeted by yield return.  These enumerators will
    /// either succeed one or fail, and are pooled so they don't allocate storage in steady state.
    ///
    /// Notes:
    /// - There is one pool shared across PrologContexts.
    /// - This could be further optimized by making a specialized fail enumerator, since there need
    ///   only be one of it.  This might possibly improve cache locality.
    /// </summary>
    internal sealed class CutStateSequencer : IEnumerable<CutState>, IEnumerator<CutState>
    {
        /// <summary>
        /// Returns a sequencer that succeeds once.
        /// </summary>
        public static CutStateSequencer Succeed()
        {
            return FromBoolean(true);
        }

        /// <summary>
        /// Returns a sequencer that fails.
        /// </summary>
        public static CutStateSequencer Fail()
        {
            return FromBoolean(false);
        }

        public static CutStateSequencer FromBoolean(bool succeed)
        {
            var s = Pool.Allocate();
            s.succeedNextCall = succeed;
            return s;
        }

        public void Dispose()
        {
            Pool.Deallocate(this);
        }

        private CutStateSequencer() 
        { }

        private static readonly StoragePool<CutStateSequencer> Pool = new StoragePool<CutStateSequencer>(() => new CutStateSequencer()); 

        private bool succeedNextCall;

        public CutState Current
        {
            get
            {
                return CutState.Continue;
            }
        }

        public void Reset()
        {
            throw new NotImplementedException();
        }

        object IEnumerator.Current
        {
            get
            {
                return Current;
            }
        }

        public bool MoveNext()
        {
            var r = succeedNextCall;
            succeedNextCall = false;
            return r;
        }

        public IEnumerator<CutState> GetEnumerator()
        {
            return this;
        }

        IEnumerator IEnumerable.GetEnumerator()
        {
            return this;
        }
    }
}
