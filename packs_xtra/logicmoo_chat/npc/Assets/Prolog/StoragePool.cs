using System;
using System.Collections.Generic;

namespace Prolog
{
    /// <summary>
    /// A pool of explicitly managed objects.
    /// Prevents GC of frequently created objects if the programmer can reliably
    /// know when to deallocate them.
    /// </summary>
    /// <typeparam name="T">Type of object to be managed.</typeparam>
    public sealed class StoragePool<T> 
        where T : class
    {
        /// <summary>
        /// Creates a new StoragePool given a constructor for new objects.
        /// </summary>
        /// <param name="createFunc">Constructor to use to create objects in the pool.</param>
        public StoragePool(Func<T> createFunc)
        {
            create = createFunc;
        }

        private readonly Stack<T> pool = new Stack<T>();

        private readonly Func<T> create; 

        /// <summary>
        /// Allocates an object frm the pool, or creates a new one if no existing objects are
        /// available.
        /// </summary>
        /// <returns>The allocated object</returns>
        public T Allocate()
        {
            if (pool.Count > 0)
                return pool.Pop();
            return create();
        }

        /// <summary>
        /// Returns object to the storage pool for reuse.
        /// </summary>
        /// <param name="item"></param>
        public void Deallocate(T item)
        {
            pool.Push(item);
        }
    }
}
