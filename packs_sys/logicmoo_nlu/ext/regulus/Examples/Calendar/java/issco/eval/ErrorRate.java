package issco.eval;

/**
 * Interface for error rates.
 * 
 * @author Maria GEORGESCUL, 
 * @copyright (C) 2008 Maria Georgescul, ISSCO/TIM, ETI, UNIVERSITY OF GENEVA
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 */

public interface ErrorRate {
	
	/**
	* @model 
	*/
	String[] getRef();
	
	
	/**
	 * Sets the value of the '{@link issco.eval.ErrorRate#getRef <em>Ref</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Ref</em>' attribute.
	 * @see #getRef()
	 * @generated
	 */
	void setRef(String value);

	/**
	 * 
	 * @model 
	 */
	String[] getHyp();
	
	/**
	 * Sets the value of the '{@link issco.eval.ErrorRate#getHyp <em>Hyp</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Hyp</em>' attribute.
	 * @see #getHyp()
	 * @generated
	 */
	void setHyp(String value);

	
	/**
	 * @model
	 */
	float computeNumerator();

	/**
	* @model 
	*/
	float computeDenominator();
		

}
