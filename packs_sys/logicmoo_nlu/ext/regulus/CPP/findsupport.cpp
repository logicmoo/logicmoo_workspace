// Compile this with
//
//   g++ -o findsupport.exe findsupport.cpp
//
// or, to get debugging output,
//
//   g++ -o findsupport.exe -DDEBUG findsupport.cpp
//
// To run it, see the call in $REGULUS/Prolog/regulus_expand.pl.
//
// Note: the -DDEBUG version won't work with regulus_expand because
// it writes a whole lot of stuff to stdout that the regulus_expand
// code doesn't expect.

// Containers we'll want to use...
#include <vector>
#include <set>
#include <map>
#include <string>

// Stdin/out and file I/O packages...
#include <iostream>
#include <fstream>

// Have to do this so we don't have to prefix all the package stuff with std:: ...
using namespace std;

// DMAssoc is shorthand for a map from integers (daughter IDs) to vectors of
// pairs of integers (mother IDs and rule IDs respectively).
typedef map<int,vector<pair <int,int> > > DMAssoc;

void readDaughterMotherAssoc(const string &fname, DMAssoc &dmAssoc, map<int,int> &rmAssoc);
int readInitialSupportedCats(const string &fname, set<int> &initialSupportedCats);
void readRuleSupport(const string &fname, vector<int> &ruleSupport);

// If this gets much bigger, we should really put what's above this point in a separate
// header file...

// Args should be: Daughter-Mother assoc file, RuleSupport assoc file, SupportedCat file.
int main(int argc, char* argv[]) {
  // Check we have the right number of args.
  if (argc != 4) {
    cerr << "Usage: " << argv[0] 
	 << " DaughterMotherAssocFile RuleSupportAssocFile SupportedRuleFile" << endl;
    return 1;
  }
  // dmAssoc maps each daughter cat ID D onto a vector of pairs of mother cat IDs M and
  // rule IDs R, meaning that D is a daughter in rule R whose mother is M.
  DMAssoc dmAssoc;
  // We record a map from rule IDs to mother IDs; it's convenient to set it
  // up when we read in dmAssoc.
  map<int,int> rmAssoc;
  readDaughterMotherAssoc(argv[1],dmAssoc,rmAssoc);
  // ruleSupport maps each ruleID onto the number of its unsupported daughters.
  vector<int> ruleSupport;
  readRuleSupport(argv[2],ruleSupport);
  // currentCats is initially the set of cats that we know have support.
  set<int> currentCats;
  int maxCatIndex = readInitialSupportedCats(argv[3],currentCats);
  // catIsSupported is our repository of whether each cat has support. We define it
  // as a vector<bool> because we want to get at it via the category IDs, not to
  // iterate through it as we do with currentCats. Initially we pretend we don't
  // know any cats have support; but the first thing we'll do in the loop is
  // to propagate currentCats into it.
  vector<bool> catIsSupported;
  for (int i=0; i<=maxCatIndex; i++) {
    catIsSupported.push_back(false);
  }
  int nIters=0;
  // And here we go with the main loop. We'll stop when an iteration produces
  // no more currentCats.
  while (!currentCats.empty()) {
#ifdef DEBUG
    cout << endl << "Starting a new iteration..." << endl;
#endif
    nIters ++;
    // 1. Set catIsSupported(c) = true for all c in currentCats
    for (set<int>::iterator it = currentCats.begin(); it != currentCats.end(); it++) {
#ifdef DEBUG
      cout << "catIsSupported to true for " << *it << endl;
#endif
      catIsSupported[*it] = true;
    }
    // This will be the next incarnation of currentCats.
    set<int> futureCats;
    // 2. Use dmAssoc to find rules r such that r has a daughter d in
    // currentCats, and catIsSupported[m] = false where m is the
    // mother of r.
    for (set<int>::iterator it = currentCats.begin(); it != currentCats.end(); it++) {
      int d = *it;
#ifdef DEBUG
      cout << "Processing currentCats member " << d << endl;
#endif
      vector<pair<int,int> > &vec = dmAssoc[d];
      for (int i=0; i<vec.size(); i++) {
	int m = vec[i].first;
	//if (!catIsSupported[m]) {
	  int r = vec[i].second;
	  // 3. Set ruleSupport[r] = ruleSupport[r] - 1, and if ruleSupport[r] is
	  // now zero, add the rule's mother to futureCats (the set of Cats which
	  // have just found support).
          ruleSupport[r] --;
#ifdef DEBUG
	  cout << "ruleSupport[" << r << "] is now " << ruleSupport[r] << " because of cat " << d << endl;
#endif
	  if (ruleSupport[r] == 0 && !catIsSupported[m]) {
#ifdef DEBUG
	    cout << "Inserting " << m << " into futureCats because of rule " << r << endl;
#endif
	    futureCats.insert(m);
	  }
	  //}
      }
    }
    // 4. Set currentCats to futureCats, ready for the next iteration. (There's
    // probably a more efficient way to do this than copying, but I can't remember
    // what it is).
    currentCats = futureCats;
  }
  // Output the number of iterations we did.
  cout << "n(" << nIters << ")." << endl;
  // Finally we output (to stdout) the rule support.
  for (int i=0; i<ruleSupport.size(); i++) {
    cout << "s(" << i << "," << ruleSupport[i] << ")." << endl;
  }
}

int readInitialSupportedCats(const string &fname, set<int> &currentCats) {
  ifstream in(fname.c_str());
  int maxCatIndex;
  in >> maxCatIndex;
  while (in) {
    int catID;
    in >> catID;
    if (in) {
      currentCats.insert(catID);
#ifdef DEBUG
      cout << "Initial supported cat: " << catID << endl;
#endif
    }
  }
  in.close();
  return maxCatIndex;
}

void readDaughterMotherAssoc(const string &fname, DMAssoc &dmAssoc, map<int,int> &rmAssoc) {
  ifstream in(fname.c_str());
  while (in) {
    int dtrID,nPairs;
    in >> dtrID >> nPairs;
    for (int n=0; n<nPairs; n++) {
      int mthrID,ruleID;
      in >> mthrID >> ruleID;
      if (in) {
	dmAssoc[dtrID].push_back(make_pair(mthrID,ruleID));
	rmAssoc[ruleID] = mthrID;
      }
    }
  }
  in.close();
}

void readRuleSupport(const string &fname, vector<int> &ruleSupport) {
  ifstream in(fname.c_str());
  while (in) {
    int nSup;
    in >> nSup;
    if (in) {
      ruleSupport.push_back(nSup);
    }
  }
  in.close();
}
