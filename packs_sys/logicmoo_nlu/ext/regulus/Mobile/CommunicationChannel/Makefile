# Include the common variables.

include $(MAEMO_ISSCO_HOME)/Common/Makefile.variables

# Include ACE macros and targets

include $(ACE_ROOT)/include/makeinclude/wrapper_macros.GNU
#include $(ACE_ROOT)/include/makeinclude/macros.GNU
#include $(ACE_ROOT)/include/makeinclude/rules.common.GNU
#include $(ACE_ROOT)/include/makeinclude/rules.nonested.GNU
#include $(ACE_ROOT)/include/makeinclude/rules.bin.GNU
#include $(ACE_ROOT)/include/makeinclude/rules.local.GNU

#
# Module name.
#

MODULE_NAME = CommunicationChannel

# Header related definitions.

EXPORTED_HEADERS = \
		AudioStreamEngineObserver.hpp \
		AudioStreamEngine.hpp \
		CommunicationChannel.hpp \
		CommunicationChannelObserver.hpp \
		CoreMemory.hpp \
		CoreTraits.hpp \
		ExceptionClass.hpp \
		MrcpControllerDefs.hpp \
		MrcpControllerObserver.hpp \
		MrcpController.hpp \
		MrcpError.hpp \
		NetPool.hpp \
		NtpTime.hpp \
		RecognitionGrammar.hpp \
		RegulusControllerObserver.hpp \
		RegulusController.hpp \
		RegulusResult.hpp \
		RegulusDialogueResult.hpp \
		RegulusTranslationResult.hpp \
		RtpPacketTypes.hpp \
		RtpPacket.hpp \
		RtpTransceiverObserver.hpp \
		RtpTransceiver.hpp \
		RtpTypes.hpp \
		RtspMessage.hpp \
		RtspRequest.hpp \
		RtspResponse.hpp \
		RtspUrl.hpp \
		SocketClass.hpp \
		SocketObserver.hpp \
		SocketRead.hpp \
		SocketsEngineObserver.hpp \
		SocketsEngine.hpp \
		SocketServer.hpp \
		SocketWrite.hpp \
		TcpClient.hpp \
		TcpClientObserver.hpp \
		TcpServer.hpp \
		TcpServerObserver.hpp \
		TcpSocket.hpp \
		UdpSocket.hpp \
		UtilsAllocator.hpp \
		UtilsBuffer.hpp \
		UtilsBuffer.inl \
		UtilsListBuffer.hpp \
		UtilsListBuffer.inl
		
# Library related definitions.

LIB_NAMES = CommunicationChannel

LIB_CommunicationChannel_SOURCES = \
		$(SRCDIR)/AudioStreamEngine.cpp \
		$(SRCDIR)/CommunicationChannel.cpp \
		$(SRCDIR)/CoreMemory.cpp \
		$(SRCDIR)/MrcpController.cpp \
		$(SRCDIR)/MrcpTransactionExecutor.cpp \
		$(SRCDIR)/MrcpTransactionStates.cpp \
		$(SRCDIR)/NetPool.cpp \
		$(SRCDIR)/NtpTime.cpp \
		$(SRCDIR)/RecognitionGrammar.cpp \
		$(SRCDIR)/RegulusController.cpp \
		$(SRCDIR)/RegulusResult.cpp \
		$(SRCDIR)/RegulusDialogueResult.cpp \
		$(SRCDIR)/RegulusTranslationResult.cpp \
		$(SRCDIR)/RtpPacketTypes.cpp \
		$(SRCDIR)/RtpPacket.cpp \
		$(SRCDIR)/RtpTransceiver.cpp \
		$(SRCDIR)/RtspUrl.cpp \
		$(SRCDIR)/RtspMessage.cpp \
		$(SRCDIR)/RtspRequest.cpp \
		$(SRCDIR)/RtspResponse.cpp \
		$(SRCDIR)/SocketClass.cpp \
		$(SRCDIR)/SocketRead.cpp \
		$(SRCDIR)/SocketsEngine.cpp \
		$(SRCDIR)/SocketServer.cpp \
		$(SRCDIR)/SocketWrite.cpp \
		$(SRCDIR)/TcpServer.cpp \
		$(SRCDIR)/TcpClient.cpp \
		$(SRCDIR)/TcpSocket.cpp \
		$(SRCDIR)/TestCommunicationChannel.cpp \
		$(SRCDIR)/UdpSocket.cpp

LIB_CommunicationChannel_LDFLAGS = -lSDL -lSDL_net -lACE -lasound

# Executable related definitions.

# EXE_NAMES = test_communicationchannel chat
EXE_NAMES = test_communicationchannel

EXE_test_communicationchannel_SOURCES = $(SRCDIR)/test.cpp
EXE_test_communicationchannel_LDFLAGS = -lCommunicationChannel -lACE -lasound -lm -ldl -lrt
#										-lhildonwidgets -lgtk-x11-2.0 \
#										-lgdk-x11-2.0 -latk-1.0 \
#										-lgdk_pixbuf-2.0 -lm -lpangoxft-1.0 \
#										-lpangox-1.0 -lpango-1.0 \
#										-lgobject-2.0 -lgmodule-2.0 \
#										-ldl -lglib-2.0

EXE_chat_SOURCES = $(SRCDIR)/chat.cpp
EXE_chat_LDFLAGS = -lSDL_net -lGUI

# Additional attributes

ADDITIONAL_CCFLAGS 		= 
ADDITIONAL_CXXFLAGS 	= 	-I/usr/include/SDL -I/usr/local/include/SDL -I$(ACE_ROOT) -I/usr/include/libxml2
#-ansi -I/usr/include/gtk-2.0 \
#							-I/usr/lib/gtk-2.0/include \
#							-I/usr/include/atk-1.0 \
#							-I/usr/include/pango-1.0 \
#							-I/usr/include/freetype2 \
#							-I/usr/include/glib-2.0 \
#							-I/usr/lib/glib-2.0/include
ADDITIONAL_LDFLAGS		=
ADDITIONAL_INCLUDES		=
ADDITIONAL_LIBRARIES	=	-L/usr/local/lib -L/usr/lib
# Include the common targets.

include $(MAEMO_ISSCO_HOME)/Common/Makefile.targets

__prebuild__:
		@-:
