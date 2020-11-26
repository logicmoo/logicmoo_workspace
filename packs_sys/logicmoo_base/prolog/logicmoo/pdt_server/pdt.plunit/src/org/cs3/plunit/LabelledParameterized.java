/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2004-2012, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/

package org.cs3.plunit;


import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.junit.runner.Description;
import org.junit.runners.Parameterized;


public class LabelledParameterized extends Parameterized {


    private List<String> labels;

    private List<String> testLabels;

    private List<Description> suiteDescriptions;

    private Map<String, Description> fileSuite = new HashMap<String, Description>();

    private Map<String, Map<String,Description>> fileSuites = new HashMap<String, Map<String,Description>>();

    private Description labelledDescription;


    public LabelledParameterized(Class<?> cl) throws Throwable {
        super(cl);
        initialiseLabels();
        generateLabelledDescription();
    }


    private void initialiseLabels() throws Exception {
        Collection<Object[]> parameterArrays = getParameterArrays();
        labels = new ArrayList<String>();
        testLabels = new ArrayList<String>();
        suiteDescriptions = new ArrayList<Description>();
        for (Object[] parameterArray : parameterArrays) {
            String fileName = parameterArray[3].toString();
            // parameterArray[1].toString()
            suiteDescriptions.add(addToSuiteAndGetUnitDescription(fileName,parameterArray));
            labels.add(fileName);
            testLabels.add(parameterArray[0].toString());
        }


    }


    private Description addToSuiteAndGetUnitDescription(String fileName, Object[] parameterArray) {
		String module = parameterArray[1].toString();
		if(fileSuite.get(fileName)==null){
			fileSuite.put(fileName,Description.createSuiteDescription(fileName));
			HashMap<String, Description> suiteMapping = new HashMap<String,Description>();
			fileSuites.put(fileName,suiteMapping);
		}
		if(fileSuites.get(fileName).get(module)==null){
			Description description = Description.createSuiteDescription(module);
			Map<String, Description> suiteMapping = fileSuites.get(fileName);
			suiteMapping.put(module, description);
			fileSuites.get(fileName).put(module,description);
			fileSuite.get(fileName).addChild(description);
			return description;
		}
		return fileSuites.get(fileName).get(module);
	}


	private Collection<Object[]> getParameterArrays() throws Exception {
        Method testClassMethod = getDeclaredMethod(this.getClass(),
                "getTestClass");
        Class<?> returnType = testClassMethod.getReturnType();
        if (returnType == Class.class)
            return getParameterArrays4_3();
        
        return getParameterArrays4_4();
    }


    private Collection<Object[]> getParameterArrays4_3() throws Exception {
        Object[][] methodCalls = new Object[][] { new Object[] { "getTestClass" } };
        Class<?> cl = invokeMethodChain(this, methodCalls);
        Method[] methods = cl.getMethods();


        Method parametersMethod = null;
        for (Method method : methods) {
            boolean providesParameters = method
                    .isAnnotationPresent(Parameters.class);
            if (!providesParameters)
                continue;


            if (parametersMethod != null)
                throw new Exception(
                        "Only one method should be annotated with @Labels");


            parametersMethod = method;
        }


        if (parametersMethod == null)
            throw new Exception("No @Parameters method found");


        Collection<Object[]> parameterArrays = (Collection<Object[]>) parametersMethod
                .invoke(null);
        return parameterArrays;


    }


    private Collection<Object[]> getParameterArrays4_4() throws Exception {
        Object[][] methodCalls = new Object[][] {
                new Object[] { "getTestClass" },
                new Object[] { "getAnnotatedMethods", Class.class,
                        Parameters.class },
                new Object[] { "get", int.class, 0 },
                // use array type for varargs (equivalent (almost))
                new Object[] { "invokeExplosively", Object.class, null,
                        Object[].class, new Object[] {} } };
        Collection<Object[]> parameterArrays = invokeMethodChain(this,
                methodCalls);
        return parameterArrays;
    }


    private <T> T invokeMethodChain(Object object, Object[][] methodCalls)
            throws Exception {
        for (Object[] methodCall : methodCalls) {
            String methodName = (String) methodCall[0];
            int parameterCount = (methodCall.length - 1) / 2;
            Class<?>[] classes = new Class<?>[parameterCount];
            Object[] arguments = new Object[parameterCount];
            for (int i = 1; i < methodCall.length; i += 2) {
                Class<?> cl = (Class<?>) methodCall[i];
                Object argument = methodCall[i + 1];
                int index = (i - 1) / 2; // messy!
                classes[index] = cl;
                arguments[index] = argument;
            }
            Method method = getDeclaredMethod(object.getClass(), methodName,
                    classes);
            object = method.invoke(object, arguments);
        }
        return (T) object;
    }


    // iterates through super-classes until found. Throws NoSuchMethodException
    // if not
    private Method getDeclaredMethod(Class<?> cl, String methodName,
            Class<?>... parameterTypes) throws NoSuchMethodException {
        do {
            try {
                Method method = cl
                        .getDeclaredMethod(methodName, parameterTypes);
                return method;
            } catch (NoSuchMethodException e) {
                // do nothing - just fall through to the below
            }
            cl = cl.getSuperclass();
        } while (cl != null);
        throw new NoSuchMethodException("Method " + methodName
                + "() not found in hierarchy");
    }


    private void generateLabelledDescription() throws Exception {
        Description originalDescription = super.getDescription();
        labelledDescription = Description
                .createSuiteDescription("plunit");
        ArrayList<Description> childDescriptions = originalDescription
                .getChildren();
        int childCount = childDescriptions.size();
        if (childCount != labels.size())
            throw new Exception(
                    "Number of labels and number of parameters must match.");

        for (Description description : fileSuite.values()) {
            labelledDescription.addChild(description);
		}

        for (int i = 0; i < childDescriptions.size(); i++) {
            Description childDescription = childDescriptions.get(i);
//            String label = labels.get(i);
//            
            Description suiteDescription = suiteDescriptions.get(i);
//            = Description
//                    .createSuiteDescription(label);
            ArrayList<Description> grandChildren = childDescription
                    .getChildren();
            for (Description grandChild : grandChildren){
            	//grandChild.getClass().getField("fDisplayName").set(grandChildren,testLabels.get(i) );
//            	Description newGrandChild = Description
//                        .createSuiteDescription(
//                        		//testLabels.get(i),
//                        		grandChild.getDisplayName(),
//                        		(java.lang.annotation.Annotation[]) grandChild.getAnnotations().toArray(new java.lang.annotation.Annotation[0]));
            	suiteDescription.addChild(grandChild);
            }
           // labelledDescription.addChild(newDescription);
        }
    }


    @Override
    public Description getDescription() {
        return labelledDescription;
    }


}


