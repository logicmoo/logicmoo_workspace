Version: 0.5

Overview:

   The scripts provided here are not production quality and have not 
been tested for secure environments.  These scripts are provided as 
sample code for developer testing and to demonstrate what a server needs 
to provide as a response to a Flash Player socket policy request.
	It is not necessary to install or use all the files provided 
within the zip file.  The scripts all perform the same basic function.  
The differences are in whether the server is run as a standalone process 
or whether the server is registered with a server process through init or 
xinetd. There are also minor differences in logging.  If the init or 
xinetd option is chosen, then the system administrator may need to 
perform a few manual steps such as adding the service to the 
/etc/services file.  The standalone scripts should be compatible with 
Cygwin on Windows operating systems.  All of the scripts require 
administrator privileges to run.
	The policyfile.xml file is a Flash Player socket master policy 
file that will need to be configured to reflect the permissions for the 
server.  The schema for the file is defined here:

http://www.adobe.com/xml/schemas/PolicyFileSocket.xsd

	Python 2.5 is required for the Python based scripts.


Usage:

   The standalone scripts can be run using the following command lines:

   ./flashpolicyd.pl --file=../policyfile.xml --port=843
   ./flashpolicyd.py --file=../policyfile.xml --port=843

   Flags
     --port: An optional argument.  If it is not provided, then the 
scripts will default to port 843.

     --file: A mandatory argument pointing to the location of the Flash 
socket policy file for the server.


Command line testing:
    A basic test of the servers can be performed by issuing the
following Perl or Python commands along with the netcat utility.  If
the policy file is returned, then the test was a success:

python -c 'print "<policy-file-request/>%c" % 0' | nc 127.0.0.1 843
perl -e 'printf "<policy-file-request/>%c",0' | nc 127.0.0.1 843



Folder layout:
   The following is a breakdown of the files present within the zip 
file:

   Python_xinetd folder
	install.sh: This file copies the files into the necessary 
directories in order to be registered as a service.  Please review the
script before running it to ensure the paths are compatible with your
system.

	in.flashpolicyd.py: This script is copied into the /usr/local/sbin 
directory by the install script for use with xinetd. It is a policy file 
daemon coded in Python and logs to syslog.

	flashpolicyd.xinet: This xinetd control file registers the 
in.flashpolicyd.py daemon with xinetd to run on port 843 as the user 
nobody. It is copied into the /etc/xinetd.d directory as 
/etc/xinetd.d/flashpolicyd by the install script.


   Perl_xinetd folder
	install.sh: This file copies the files into the necessary 
directories in order to be registered as a service. Please review the
script before running it to ensure the paths are compatible with your
system.

	in.flashpolicyd.pl: This script is copied into the /usr/local/sbin 
directory by the install script for use with xinetd. It is a policy file 
daemon coded in Perl and logs to STDOUT and STDERR.

	flashpolicyd.xinet: This xinetd control file registers the 
in.flashpolicyd.py daemon with xinetd to run on port 843 as the user 
nobody. It is copied into the /etc/xinetd.d directory as 
/etc/xinetd.d/flashpolicyd by the install script.


   Python_init folder
	install.sh: This file copies the files into the necessary 
directories in order to be registered as a service.  Please review the
script before running it to ensure the paths are compatible with your
system.

	flashpolicyd.py: This script is copied into the /usr/local/sbin 
directory by the install script. It is an init compatible version of the 
Flash policy server that logs to STDERR. This version supports more than 
one user connection.

	flashpolicyd.init: This init script supports the basic init 
commands of "start", "stop" and "restart". It is copied into the 
/etc/rc.d/init.d directory as /etc/rc.d/init.d/flashpolicyd by the 
install script. If this method is used, then the service will be run as 
root. Be sure to review the global variables defined at the beginning of
the script to ensure that the paths are compatible with your OS. 


   Standalone folder
	flashpolicyd.pl: This is a standalone, single-threaded Perl script that 
runs in a loop accepting one user connection at a time until the user 
hits Ctrl-C.

	flashpolicyd.py:  This standalone Python Script will run in a loop 
until the user hits Ctrl-C. This version supports more than one user 
connection.


   flashpolicy.xml: This is the socket master policy file that is 
copied into /usr/local/etc/ by the install scripts. This file must be 
altered to reflect the relevant ports and domains for the server.

   README.txt:  A file that contains information regarding the
applications and this project.




License Agreement:
Adobe Systems Incorporated(r) Source Code License Agreement
Copyright(c) 2005 Adobe Systems Incorporated. All rights reserved.
    
Please read this Source Code License Agreement carefully before using 
the source code.
    
Adobe Systems Incorporated grants to you a perpetual, worldwide,
non-exclusive, no-charge, royalty-free, irrevocable copyright license,
to reproduce, prepare derivative works of, publicly display, publicly 
perform, and distribute this source code and such derivative works in
source or object code form without any attribution requirements.
    
The name "Adobe Systems Incorporated" must not be used to endorse or 
promote products derived from the source code without prior written
permission.
    
You agree to indemnify, hold harmless and defend Adobe Systems
Incorporated from and against any loss, damage, claims or lawsuits,
including attorney's fees that arise or result from your use or
distribution of the source code.
    
THIS SOURCE CODE IS PROVIDED "AS IS" AND "WITH ALL FAULTS", WITHOUT ANY
TECHNICAL SUPPORT OR ANY EXPRESSED OR IMPLIED WARRANTIES, INCLUDING,
BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. ALSO, THERE IS NO 
WARRANTY OF NON-INFRINGEMENT, TITLE OR QUIET ENJOYMENT. IN NO EVENT
SHALL MACROMEDIA OR ITS SUPPLIERS BE LIABLE FOR ANY DIRECT, INDIRECT, 
INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF 
USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOURCE CODE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
