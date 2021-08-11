---+ SWI-Prolog License Conditions (prior to 7.3.33)

**
This file describes the license conditions up to SWI-Prolog 7.3.32.
Later versions are distributed under the permissive Simplified BSD
license.  See [license](<license.html>).
**

---++ Preamble

SWI-Prolog licensing aims at a large audience, combining ideas from the
Free Software Foundation and the less principal Open Source Initiative.
The license aims at the following:

    * Make SWI-Prolog itself and its libraries `As free as possible'.
    * Allow for easy integration of contributions.
    * Free software can build on SWI-Prolog without limitations.
    * Non-free (open or proprietary) software can be produced using
    SWI-Prolog, although contributed pure GPL components cannot be used.

To achieve this, different parts of the system have different licenses.
SWI-Prolog programs consist of a mixture of `native' code (source
compiled to machine instructions) and `virtual machine' code (Prolog
source compiled to SWI-Prolog virtual machine instructions, covering
both compiled SWI-Prolog libraries and your compiled application).

---+++ The SWI-Prolog kernel and foreign libraries

The SWI-Prolog kernel and our foreign libraries are distributed under
the Lesser GNU Public License, also called the LGPL. A Prolog executable
consists of the combination of these `native' code components and Prolog
virtual machine code. The SWI-Prolog plrc utility allows for
disassembling and re-assembling these parts, a process satisfying
article 6b of the LGPL.

Under the LGPL SWI-Prolog can be linked to code distributed under
arbitrary licenses, provided a number of requirements are fulfilled.
The most important requirement is that if an application relies on a
modified version of SWI-Prolog, the modified sources must be made
available.

---+++ Contributed foreign libraries

Foreign libraries contributed to the SWI-Prolog project must have
license conditions that are compatible with the LGPL or the GPL.
Applications using SWI-Prolog must satisfy the license restrictions of
all modules.

---+++ The SWI-Prolog Prolog libraries

Lacking a satisfactory technical solution to handle article 6 of the
LGPL, this license cannot be used for the Prolog source code that is
part of the SWI-Prolog system (both libraries and kernel code). This
situation is comparable to libgcc, the runtime library used with the GNU
C-compiler. Therefore, we use the same proven license terms as this
library. The libgcc license is the GPL, accompanied with a special
exception. Below we rephrased this exception adjusted to our needs:

As a special exception, if you link this library with other files
compiled with a Free Software compiler to produce an executable, this
library does not by itself cause the resulting executable to be covered
by the GNU General Public License. This exception does not, however,
invalidate any other reasons why the executable file might be covered by
the GNU General Public License.

---++ Keeping it free, open and practical

The above sounds a bit complicated. To facilitate this heterogeneous
license system we have added some machinery to SWI-Prolog:

    * [[license/1]]
    * [[license/0]]

Foreign language modules can declare their license conditions using:

    * PL_license(const char *module-id, enum PL_license_t license)
    Allows for registering license information from foreign code through
    the foreign-language interface. For example, the GNU readline
    library is linked to the system using

    ==
        ...
        PL_license("libreadline", LICENSE_GPL);
        ...
    ==


---++ Contributing to the SWI-Prolog project

To achieve maximal coherence using SWI-Prolog for Free and Non-Free
software we advise using the LGPL for contributed foreign code and
using the GPL with SWI-Prolog exception for Prolog code for
contributed modules.

As a rule of thumb it is advised to use the above licenses whenever
possible and only use a strict GPL compliant license if the module
contains other code under strict GPL compliant licenses.
