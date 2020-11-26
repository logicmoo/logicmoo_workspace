/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: Lukas Degener (among others)
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2004-2012, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/

package org.cs3.prolog.connector.internal.process.socket;

/**
 * marks an object as being reusable.
 * 
 * Lifcycle of an reusable object looks like this:
 * Once the instance is not used any more, recycle() is 
 * called on it.
 * This method should hand over the Reusable instance with some kind of instance
 * pool, e.g. PrologSessionPool. After that there the using code should take 
 *   care to not further reference the instance.
 * <p>
 * the pool will eventualy decide about the future of the object and either call
 * destroy() or reuse().
 *
 */
public interface Reusable {
    /**
     * reuse an recycled instance.
     *
     *should reset the object's state so that it can be used by another instance.
     */    
    public void reuse();
    
    /**
     * destroy an recycled instance
     */
    public void destroy();
    
    /**
     * recycle an instance.     
     */
    public void recylce();
}


