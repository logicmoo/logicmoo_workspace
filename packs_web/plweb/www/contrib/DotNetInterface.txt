---+ CSharp for a .NET 2.0 web service

Below is a *very* basic recipe, written in CSharp as a .NET 2.0 web
service, which utilizes the System.Diagnostics.Process namespace in
order to wrap the plcon.exe and use it as a prolog server. This was all
tested in XP Professional, and you need to make sure that you edit the
config file as appropriate. Also, you will need to make sure that the
impersonation block in the web.config is impersonating a user with
read/write priviledges to App_Data.

The Basic tests which were run are as follows:

  1. An assertion of 100,000 facts from one process web service call,
     while querying through another process call. It appeared that it
     was possible to query the system as the new facts were added.

  2. Spawned 10 console processes which repeatedly queried the service
     1000 times a piece (this took about 2-3 minutes) you will need to
     run this test on your machine, performance time could vary greatly
     dependent upon memory and CPU power.

As tested, this solution appears capable of supporting a few clients
reliably. Since it is all packaged in a single web service, you should
be able to horizontally scale the solution. The code is in need of
further development to make it more robust. Also, the code does not take
advantage of the built in threading capability of SWI prolog. So, I
think this toy project could be developed further.

Finally, the basic approach can be duplicated in any language which
supports OS calls to spawn and manage external processes. The solution
is as follows:

  1. Create a plcon.exe process.
  2. Redirect only its standard input. (I do nothing with stdout or
     stderr)
  3. Have prolog write responses (using Tell/Told) and use a Guid to
     label the file.
  4. Spin and Wait until the file named after the Guid appears.
  5. Open response and use
  6. Repeat.

I have used the same procedure in Ruby using POPEN and it appears very
stable.

@see also CSharp.txt
@see [[LogicService_20060623.zip][<LogicService_20060623.zip>]]
@see [[PackageProlog.rb][<PackageProlog.rb>]]
@author Daniel Sullivan
