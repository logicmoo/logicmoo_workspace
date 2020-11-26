
/* Original implementation can be found here:
 * https://stackoverflow.com/questions/19123506/jaro-winkler-distance-algorithm-in-c-sharp
 */

#include <string.h>
#include "jaro_winkler.h"

#define max(X,Y) ((X)>(Y)?(X):(Y))

#define min(X,Y) ((X)<(Y)?(X):(Y))

#define true 1
#define false 0

/* The Winkler modification will not be applied unless the 
 * percent match was at or above the mWeightThreshold percent 
 * without the modification. 
 * Winkler's paper used a default value of 0.7
 */
const double mWeightThreshold = 0.7;

/* Size of the prefix to be concidered by the Winkler modification. 
 * Winkler's paper used a default value of 4
 */
const int mNumChars = 4;

/// <summary>
/// Returns the Jaro-Winkler distance between the specified  
/// strings. The distance is symmetric and will fall in the 
/// range 0 (perfect match) to 1 (no match). 
/// </summary>
/// <param name="aString1">First String</param>
/// <param name="aString2">Second String</param>
/// <returns></returns>

double distance(char *const aString1, char *const aString2)
{
    return 1.0 - proximity(aString1, aString2);
}

typedef char bool;

/// <summary>
/// Returns the Jaro-Winkler distance between the specified  
/// strings. The distance is symmetric and will fall in the 
/// range 0 (no match) to 1 (perfect match). 
/// </summary>
/// <param name="aString1">First String</param>
/// <param name="aString2">Second String</param>
/// <returns></returns>
double proximity(char *const aString1, char *const aString2)
{
    int lLen1 = strlen(aString1);
    int lLen2 = strlen(aString2);
    if (lLen1 == 0)
        return lLen2 == 0 ? 1.0 : 0.0;

    int  lSearchRange = max(0, max(lLen1, lLen2)/2 - 1);

    // default initialized to false
    bool lMatched1[lLen1];
    bool lMatched2[lLen2];
    
    for (int i = 0; i < lLen1; ++i)
        lMatched1[i] = false;
    for (int i = 0; i < lLen2; ++i)
        lMatched2[i] = false;
    
    int lNumCommon = 0;
    for (int i = 0; i < lLen1; ++i) {
        int lStart = max(0,i-lSearchRange);
        int lEnd = min(i+lSearchRange+1,lLen2);
        for (int j = lStart; j < lEnd; ++j) {
            if (lMatched2[j]) continue;
            if (aString1[i] != aString2[j])
                continue;
            lMatched1[i] = true;
            lMatched2[j] = true;
            ++lNumCommon;
            break;
        }
    }
    if (lNumCommon == 0) return 0.0;

    int lNumHalfTransposed = 0;
    int k = 0;
    for (int i = 0; i < lLen1; ++i) {
        if (!lMatched1[i]) continue;
        while (!lMatched2[k]) ++k;
        if (aString1[i] != aString2[k])
            ++lNumHalfTransposed;
        ++k;
    }
    // System.Diagnostics.Debug.WriteLine("numHalfTransposed=" + numHalfTransposed);
    int lNumTransposed = lNumHalfTransposed/2;

    // System.Diagnostics.Debug.WriteLine("numCommon=" + numCommon + " numTransposed=" + numTransposed);
    double lNumCommonD = lNumCommon;
    double lWeight = (lNumCommonD/lLen1
                     + lNumCommonD/lLen2
                     + (lNumCommon - lNumTransposed)/lNumCommonD)/3.0;

    if (lWeight <= mWeightThreshold) return lWeight;
    int lMax = min(mNumChars,min(lLen1,lLen2));
    int lPos = 0;
    while (lPos < lMax && aString1[lPos] == aString2[lPos])
        ++lPos;
    if (lPos == 0) return lWeight;
    return lWeight + 0.1 * lPos * (1.0 - lWeight);

}
