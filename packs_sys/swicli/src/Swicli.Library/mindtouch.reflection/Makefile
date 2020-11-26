include $(root_dir)/Makefile.include

ASSEMBLY_NAME=mindtouch.deki.util
ASSEMBLY=$(ASSEMBLY_NAME).dll

TARGET=$(addprefix $(build_prefix)/, $(ASSEMBLY))

sources = 							        \
        Properties/AssemblyInfo.cs          \
        IGenericParameterProvider.cs        \
        IReflectedInfo.cs                   \
        IsolatedTypeInspector.cs            \
        ITypeInspector.cs                   \
        MemberAccess.cs                     \
        ParameterBuilder.cs                 \
        ReflectedConstructorInfo.cs         \
        ReflectedEventInfo.cs               \
        ReflectedFieldInfo.cs               \
        ReflectedGenericParameterInfo.cs    \
        ReflectedMemberInfo.cs              \
        ReflectedMethodInfo.cs              \
        ReflectedParameterInfo.cs           \
        ReflectedParameterTypeInfo.cs       \
        ReflectedPropertyInfo.cs            \
        ReflectedTypeInfo.cs                \
        TypeInspector.cs

references = 																\
		-r:System															\
		-r:System.Core														\
		-r:System.Data														\
		-r:System.Web														\
		-r:System.Xml														\
		-r:$(addprefix $(root_dir)/, redist/log4net.dll)					\
	
$(TARGET): $(sources)
	$(MCS) $(MCS_FLAGS) /out:$(TARGET) /target:library $(references) $(sources)

all: $(TARGET)
