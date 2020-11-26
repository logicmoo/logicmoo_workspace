================================
SemLink VerbNet/FrameNet Mapping
================================

  Version: 1.1
      URL: http://verbs.colorado.edu/semlink/


VerbNet / FrameNet Mapping Data
--------------------------------
The SemLink VerbNet/FrameNet mapping consists of two parts.  The first
part is a many-to-many mapping of VerbNet Classes and FrameNet Frames.
It is many-to-many in that a given FrameNet lexical unit can map to more
than one VerbNet member, and more frequently, a given VerbNet member can
map to more than one FrameNet Frame.  The second part is a mapping of
VerbNet ThematicRoles and FrameNet FrameElements.  The two parts have
been provided in separate files in order to offer the cleanest possible
formatting.

When the mapping efforts were begun, FrameNet v1.2 was the latest available
release.  Now that v1.3 has been released, work is being done to update
the mappings provided in these files.  Now that v1.5 has been released, more work is being done to update these mappings.


VerbNet Class / FrameNet Frame Mapping
--------------------------------------
The Class/Frame mapping file lists elements that include these 
attibutes:

	class      --  VerbNet class ID (numeric)
	vnmember   --  VerbNet class member (string, the verb lemma)
	fnframe    --  FrameNet Frame (string)
	fnlexent   --  FrameNet lexical entry ID (numeric)
	versionID  --  VerbNet version ID (either 1.5 or 2.0)

The structure of an element can be demonstrated by this example:

  <vncls class='9.1-2' vnmember='put' fnframe='Placing' fnlexent='5355' versionID='vn2.0' />

Note that the string values for 'fnframe' and 'vnmember' attributes 
can include hyphens and underscores.

There are two special values possible for the 'fnframe' attribute:

	fnframe='DS'  -- Different Sense
	fnframe='NA'  -- Not Available

"Different Sense" covers cases where a particular VerbNet lemma exists
as the word form of one or more Lexical Units inFrameNet, but none that
share the lexical semantics of the VerbNet Class member closely enough.

"Not Available" covers cases where a VerbNet lemma doesn't exist as a
word form at all in FrameNet.

The 'fnlexent' attribute provides the lexical entry ID number FrameNet 
assigned to the verb.


VerbNet ThematicRole / FrameNet FrameElement Mapping
----------------------------------------------------
The thematic role / frame element mapping file includes the possible 
role correspondences for the VerbNet Classes and FrameNet Frames 
that have been mapped.  The number of role mappings depends on the 
particular Class and Frame, and so will vary in number.

The two new attributes included here are the following:

	fnrole     -- FrameNet frame element (string)
	vnrole     -- VerbNet thematic role (string)

The structure of an element can be demonstrated by this example:

  <vncls class='9.1' fnframe='Placing'>
    <roles>
      <role fnrole='Agent' vnrole='Agent'/>
      <role fnrole='Cause' vnrole='Agent'/>
      <role fnrole='Goal' vnrole='Destination'/>
      <role fnrole='Theme' vnrole='Theme'/>
    </roles>
  </vncls>

VerbNet classes inherit their roles from their parents.  Because of this,
the role mappings for a subclass like 51.3.2-2-1 will be the same as the role
mappings for the parent 51.3.2.  Note that the string values for the 'fnframe' 
attribute can include hyphens and underscores.
