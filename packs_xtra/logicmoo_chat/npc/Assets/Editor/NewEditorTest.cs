using UnityEngine;
using UnityEditor;
using NUnit.Framework;

public class NewEditorTest {

    [Test]
    public void EditorTest()
    {
        //Arrange
        var gameObject = new GameObject();
        //We register the newly create GameObject so it will be automatically removed after the test run
        Undo.RegisterCreatedObjectUndo (gameObject, "Created test GameObject");

        //Act
        //Try to rename the GameObject
        var newGameObjectName = "My game object";
        gameObject.name = newGameObjectName;

        //Assert
        //The object has a new name
        Assert.AreEqual(newGameObjectName, gameObject.name);
    }
}
