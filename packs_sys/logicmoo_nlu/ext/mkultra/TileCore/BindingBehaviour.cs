//#define LogBinding

using System;
using System.Collections;
using System.Collections.Generic;
using System.Reflection;
using UnityEngine;
using Object = UnityEngine.Object;

/*
 * Implementation of automatic binding of fields to components in Unity.
 * Ian Horswill, summer 2013
 *
 * Usage:
 class MyComponentClass : BindingBehaviour    // Use BindingBehavior rather than MonoBehaviour
 {
    // Field is left alone by the binding system because it isn't decorated with [Bind]
    ComponentType field;
 
    // Bind field to the ComponentType component of this component's gameObject
    // If none exists, throw an exception.
    [Bind]
    ComponentType field; 
 
    // Same, but don't throw an exception if there's no component; just leave the field null.
    [Bind(BindingDefault.Ignore)]
    ComponentType field; 
 
    // Same, but make one if there isn't one already.
    [Bind(BindingDefault.Create)]
    ComponentType field; 
 
    // Like [Bind], but search children too
    [Bind(BindingScope.GameObjectOrChildren)]
    ComponentType field;  
 
    // Same, but search the whole level for an instance
    [Bind(BindingScope.Global)]
    ComponentType field;   

    // Bind field to *all* ComponentTypes of this gameObject
    // If there are no instances, field is set to a zero-length array.
    // If new instances are created or deleted, this field is not updated.
    [Bind]
    ComponentType[] field;
 
    // Bind field to *all* ComponentTypes in this GameObject and its children
    [Bind(BindingScope.GameObjectOrChildren)]
    ComponentType[] field;
 
    // Bind field to *all* ComponentTypes in the level
    [Bind(BindingScope.Global)]
    ComponentType[] field;

    // Bind field to all ComponentType instances in the level and update it as instances are created and deleted.
    // - Assumes BindingScope.Global (sorry)
    // - ComponentType must be a subclass or BindingBehaviour, or else you must manually write Awake/OnEnable and OnDisable
    //   methods to call Register() and Unregister().
    [Bind]
    System.Collections.Generic.List<ComponentType> field;  
 }
 */

/// <summary>
/// A MonoBehavior that automatically binds field tagged with [Bind] attributes when Awake() is called.
/// Be sure to call base.Awake() if you override Awake().
/// </summary>
public abstract class BindingBehaviour : MonoBehaviour
{
    /// <summary>
    /// This exists only to defeat compiler warning claiming that an autobound field isn't initialized.
    /// WARNING: IT DOES NOT AUTOBIND THE FIELD!!!  USE A [Bind] ANNOTATION FOR THAT
    /// </summary>
    /// <typeparam name="T">The of the field</typeparam>
    /// <returns>null or 0</returns>
    public static T AutoBound<T>()
    {
        return default(T);
    }

    #region Standard Unity methods
    /// <summary>
    /// Automatically sets any non-null fields decorated with a [Bind] attribute. 
    /// </summary>
    public virtual void Awake()
    {
		//Debug.Log("Awake: "+ToString());
        this.BindAndRegister();
    }

    /// <summary>
    /// Unregisters this object from the component registries.
    /// </summary>
    public virtual void OnDisable()
    {
        Unregister(this);
    }
    #endregion

    #region Binding of fields
    /// <summary>
    /// Binds this component's bindable fields and registers it in the database of tracked BindingBehaviors.
    /// </summary>
    private void BindAndRegister()
    {
		//Debug.Log("Bind and register "+this.ToString());
        BindFields(this);
        Register(this);
    }

    /// <summary>
    /// Force rebinding of all components
    /// Intended for use in editor, since it doesn't call Awake on components.
    /// </summary>
    public static void ForceRebinding()
    {
#if LogBinding
		Debug.Log("Forcing rebinding ...");
#endif
        // ReSharper disable PossibleInvalidCastExceptionInForeachLoop
        foreach (BindingBehaviour b in FindObjectsOfType(typeof(BindingBehaviour)))
            // ReSharper restore PossibleInvalidCastExceptionInForeachLoop
            b.BindAndRegister();
    }

    /// <summary>
    /// Binds all non-null fields of component that have [Bind] annotations.
    /// This is static and takes a MonoBehaviour as an argument so you can call it manually from non-BindingBehaviours if you want to.
    /// </summary>
    /// <param name="component">Component to initialize</param>
    public static void BindFields(MonoBehaviour component)
    {
#if LogBinding
		Debug.Log("Bind fields "+component.ToString());
#endif
        var type = component.GetType();
        while (type != null)
        {
            var fields = type.GetFields(BindingFlags.Instance | BindingFlags.NonPublic | BindingFlags.Public);
            foreach (var field in fields)
                BindFieldIfNecessary(component, field);
            type = type.BaseType;
        }
    }

    /// <summary>
    /// Binds the specified field of the component if it is non-null and has a [Bind] attribute.
    /// </summary>
    private static void BindFieldIfNecessary(MonoBehaviour component, FieldInfo field)
    {
#if LogBinding
		Debug.Log("Bind if necessary "+field.ToString());
#endif
        var fieldType = field.FieldType;

        if (fieldType.IsClass && fieldType.IsSubclassOf(typeof(Component)))
            SimpleField(component, field);
        else
        {
            var elementType = fieldType.GetElementType();

            // ReSharper disable PossibleNullReferenceException
            if (fieldType.IsArray && elementType.IsSubclassOf(typeof(Component)))
                // ReSharper restore PossibleNullReferenceException
                ArrayField(component, field, elementType);
            else if (fieldType.IsGenericType && fieldType.GetGenericTypeDefinition() == typeof(List<>))
            {
                elementType = fieldType.GetGenericArguments()[0];
                if (elementType.IsSubclassOf(typeof(BindingBehaviour)))
                    ListField(component, field, elementType);
            }
        }
    }

    /// <summary>
    /// Binds a field whose type is a subclass of Component, if it does not already have a value.
    /// </summary>
    /// <param name="component">Component containing the field</param>
    /// <param name="field">The field of the component to bind</param>
    private static void SimpleField(MonoBehaviour component, FieldInfo field)
    {
        // ReSharper disable RedundantNameQualifier
        var value = (UnityEngine.Object)field.GetValue(component);
        // ReSharper restore RedundantNameQualifier
        if (value == null)
        {
            var bindAttributes = field.GetCustomAttributes(typeof(BindAttribute), true);
            switch (bindAttributes.Length)
            {
                case 0:
                    break;

                case 1:
                    BindSimpleField(field, (BindAttribute)bindAttributes[0], component);
                    break;

                default:
                    throw new Exception(
                        String.Format("Multiple BindAttributes on field {0} of component {1}", field.Name, component.name));
            }
        }
    }

    /// <summary>
    /// Binds an array of components to all the components of its type within the scope specified by the [Bind] attribute.
    /// Does nothing if the array is already non-null.
    /// </summary>
    /// <param name="component">Component containing the field</param>
    /// <param name="field">The field of the component to bind</param>
    /// <param name="elementType">Type of elements of the array.  Must be a subclass of Component.</param>
    private static void ArrayField(MonoBehaviour component, FieldInfo field, Type elementType)
    {
        if (field.GetValue(component) == null)
        {
            var bindAttributes = field.GetCustomAttributes(typeof(BindAttribute), true);
            switch (bindAttributes.Length)
            {
                case 0:
                    break;

                case 1:
                    BindArrayField(field, (BindAttribute)bindAttributes[0], component, elementType);
                    break;

                default:
                    throw new Exception(
                        String.Format("Multiple BindAttributes on field {0} of component {1}", field.Name, component.name));
            }
        }
    }

    /// <summary>
    /// Binds a System.Collections.Generic.List of components to all the components of its type within the scope specified by the [Bind] attribute.
    /// Does nothing if the field is already non-null.
    /// </summary>
    /// <param name="component">Component containing the field</param>
    /// <param name="field">The field of the component to bind</param>
    /// <param name="elementType">Type of elements of the list.  Must be a subclass of BindingBehaviour.</param>
    private static void ListField(MonoBehaviour component, FieldInfo field, Type elementType)
    {
        if (field.GetValue(component) == null)
        {
            var bindAttributes = field.GetCustomAttributes(typeof(BindAttribute), true);
            switch (bindAttributes.Length)
            {
                case 0:
                    break;

                case 1:
                    BindListField(field, (BindAttribute)bindAttributes[0], component, elementType);
                    break;

                default:
                    throw new Exception(
                        String.Format("Multiple BindAttributes on field {0} of component {1}", field.Name, component.name));
            }
        }
    }

    /// <summary>
    /// Initializes the specified field of component given then information in bindAttribute.
    /// </summary>
    /// <param name="field">Field to bind</param>
    /// <param name="bindAttribute">Information about how to bind it</param>
    /// <param name="component">Object with the field</param>
    private static void BindSimpleField(FieldInfo field, BindAttribute bindAttribute, MonoBehaviour component)
    {
        Object target = null;
        switch (bindAttribute.Scope)
        {
            case BindingScope.GameObject:
                target = component.GetComponent(field.FieldType);
                break;

            case BindingScope.GameObjectOrChildren:
                target = component.GetComponentInChildren(field.FieldType);
                break;

            case BindingScope.Global:
                target = FindObjectOfType(field.FieldType);
                break;
        }

        if (target == null)
            switch (bindAttribute.IfNotFound)
            {
                case BindingDefault.Exception:
                    throw new Exception(string.Format("{0}.{1}.{2}: no target to bind to", component.gameObject.name, component.GetType().Name, field.Name));

                case BindingDefault.Create:
                    target = component.gameObject.AddComponent(field.FieldType);
                    break;

                case BindingDefault.Ignore:
                    break;
            }

#if LogBinding
        Debug.Log(string.Format("Binding {0}.{1}.{2} to {3}", component.gameObject.name, component.GetType().Name, field.Name, target));
#endif
        field.SetValue(component, target);
    }

    /// <summary>
    /// Initializes the specified field of component to all instances of some type, given then information in bindAttribute.
    /// </summary>
    /// <param name="field">Field to bind</param>
    /// <param name="bindAttribute">Information about how to bind it</param>
    /// <param name="component">Object with the field</param>
    /// <param name="elementType">Element type of the field to be bound</param>
    private static void BindArrayField(FieldInfo field, BindAttribute bindAttribute, MonoBehaviour component, Type elementType)
    {
        Object[] target = null;
        switch (bindAttribute.Scope)
        {
            case BindingScope.GameObject:
                // ReSharper disable CoVariantArrayConversion
                target = component.GetComponents(elementType);
                // ReSharper restore CoVariantArrayConversion
                break;

            case BindingScope.GameObjectOrChildren:
                // ReSharper disable CoVariantArrayConversion
                target = component.GetComponentsInChildren(elementType);
                // ReSharper restore CoVariantArrayConversion
                break;

            case BindingScope.Global:
                target = FindObjectsOfType(elementType);
                break;
        }
#if LogBinding
        Debug.Log(string.Format("Binding {0}.{1}.{2} to {3} elements", component.gameObject.name, component.GetType().Name, field.Name, target.Length));
#endif
        field.SetValue(component, target);
    }

    /// <summary>
    /// Initializes the specified field of component to all instances of some type, given then information in bindAttribute.
    /// </summary>
    /// <param name="field">Field to bind</param>
    /// <param name="bindAttribute">Information about how to bind it</param>
    /// <param name="component">Object with the field</param>
    /// <param name="elementType">Element type of the field to be bound</param>
    // ReSharper disable UnusedParameter.Local
    private static void BindListField(FieldInfo field, BindAttribute bindAttribute, MonoBehaviour component, Type elementType)
    // ReSharper restore UnusedParameter.Local
    {
        System.Diagnostics.Debug.Assert(bindAttribute.Scope == BindingScope.Global);
#if LogBinding
        Debug.Log(string.Format("Binding {0}.{1}.{2} to {3} elements", component.gameObject.name, component.GetType().Name, field.Name, Registry(elementType).Count));
#endif
        field.SetValue(component, Registry(elementType));
    }
    #endregion

    #region Tracking of registered instances of types
    //
    // Collections of BindingBehaviours of a given type are collected in System.Collections.Generic.Lists, known as "registries".
    // When a field of type List(T) is bound using [Bind], it is set to the registry itself, so that it updates automatically
    // as instances of the type are created and deleted.  Thus all fields of a given List(T) type are set to the same underlying
    // list object, aka the registry.
    //

    /// <summary>
    /// Holds the registries for a given type.  A registry is only created if there is an object that is trying to bind to it.
    /// </summary>
    private static readonly Dictionary<Type, IList> Registries = new Dictionary<Type, IList>();

    /// <summary>
    /// Returns the registry for the given type.
    /// </summary>
    /// <param name="type">The type to get the registry for.</param>
    /// <returns>The registry for type.  This will be of type System.Collections.Generic.List(type), but since all generic lists are also ILists, we can manipulate it using the IList interface, which simplifies the code for maintaining the registry.</returns>
    public static IList Registry(Type type)
    {
        IList probe;
        if (Registries.TryGetValue(type, out probe))
            return probe;
        var registry = MakeList(type);
        var findObjectsOfType = FindObjectsOfType(type);
        foreach (var obj in findObjectsOfType)
            registry.Add(obj);
        Registries[type] = registry;
        return registry;
    }

    /// <summary>
    /// Returns the registry for the given type.
    /// </summary>
    /// <typeparam name="T">The type to get the registry for.</typeparam>
    /// <returns>The registry for type.</returns>
    public static List<T> Registry<T>()
    {
        return (List<T>)Registry(typeof(T));
    } 

    /// <summary>
    /// Creates an empty list of type System.Collections.Generic.List(T), where T is the argument type.
    /// </summary>
    /// <param name="t">The element type for the list</param>
    /// <returns>An empty list of the type.</returns>
    private static IList MakeList(Type t)
    {
        return (IList)Activator.CreateInstance(typeof(List<>).MakeGenericType(t));
    }

    /// <summary>
    /// Adds the object to the registry for type t, if there is one.
    /// </summary>
    /// <param name="t">Type under which to register the object</param>
    /// <param name="o">The object to register</param>
    private static void RegisterUnderType(Type t, Object o)
    {
        IList probe;
        if (Registries.TryGetValue(t, out probe) && !probe.Contains(o))
            probe.Add(o);
    }

    /// <summary>
    /// Removes the object from the registry for the type t.
    /// </summary>
    /// <param name="t">The type to unregister it from</param>
    /// <param name="o">The object to unregister</param>
    private static void UnregisterUnderType(Type t, Object o)
    {
        IList probe;
        if (Registries.TryGetValue(t, out probe))
            probe.Remove(o);
    }

    /// <summary>
    /// Registers the object under all its types (type, parent type, etc.)
    /// Does not register the object under interface types.
    /// </summary>
    /// <param name="c"></param>
    internal static void Register(Object c)
    {
        var t = c.GetType();
        while (t != null)
        {
            RegisterUnderType(t, c);
            t = t.BaseType;
        }
    }

    /// <summary>
    /// Unregisters the object from its types.
    /// </summary>
    /// <param name="c"></param>
    internal static void Unregister(Object c)
    {
        var t = c.GetType();
        while (t != null)
        {
            UnregisterUnderType(t, c);
            t = t.BaseType;
        }
    }

    /// <summary>
    /// Forcibly rebuilds the registry for the specified type.
    /// Intended for use in the editor, which doesn't call Awake() routines.
    /// </summary>
    /// <param name="t">The type to update the registry of.</param>
    public static void ForceRegistryUpdate(Type t)
    {
        var registry = Registry(t);
        registry.Clear();
        var findObjectsOfType = FindObjectsOfType(t);
        foreach (var obj in findObjectsOfType)
            registry.Add(obj);
    }
    #endregion
}

/// <summary>
/// Declares that a field should be automatically bound to a component and controls how the binding is performed.
/// Binding is not performed if the field is already non-null.
/// 
/// IMPORTANT: should only be applied to fields whose types are subclasses of UnityEngine.Component.
/// </summary>
public class BindAttribute : Attribute
{
    public BindAttribute(BindingScope scope=BindingScope.GameObject, BindingDefault ifNotFound = BindingDefault.Exception)
    {
        IfNotFound = ifNotFound;
        Scope = scope;
    }

    /// <summary>
    /// Action to take when binding non-collection fields if no matching component can be found.
    /// </summary>
    public readonly BindingDefault IfNotFound;
    /// <summary>
    /// Scope over which to search for components.
    /// </summary>
    public readonly BindingScope Scope;
}

/// <summary>
/// Controls how the binder responds to fields for which no matching component can be found.
/// </summary>
public enum BindingDefault
{
    /// <summary>
    /// Ignore the fact that there is no matching component for this field
    /// </summary>
    Ignore,
    /// <summary>
    /// Throw an exception if no matching component can be found for this field
    /// </summary>
    Exception,
    /// <summary>
    /// Create a component as a sibling of this one if no matching component can be found.
    /// </summary>
    Create
}

/// <summary>
/// Controls where to search for matching components when binding.
/// </summary>
public enum BindingScope
{
    /// <summary>
    /// Search the entire object hierarchy.
    /// </summary>
    Global,
    /// <summary>
    /// Search only siblings of the component being bound.
    /// </summary>
    GameObject,
    /// <summary>
    /// Search all children of the component's GameObject.
    /// </summary>
    GameObjectOrChildren
}
