#ifndef REGULUSDIALOGUERESULT_HPP_
#define REGULUSDIALOGUERESULT_HPP_

#include <string>
#include "RegulusResult.hpp"

using namespace std;

class RegulusDialogueResult : public RegulusResult
{
public:

	RegulusDialogueResult();
	RegulusDialogueResult(const string& input);
	
	~RegulusDialogueResult();
		
	void SetSelected(const string& text);
	const string& GetSelected();
	
	void SetParaphrase(const string& text);
	const string& GetParaphrase();
	
	void SetQueryOutput(const string& text);
	const string& GetQueryOutput();
	
	void ClearResult();
	void ParseInput(const string& input);

private:
	
	// Avoid accidental copy or assignment
	RegulusDialogueResult(const RegulusDialogueResult&);
	RegulusDialogueResult& operator = (const RegulusDialogueResult&);
	
private:
	
	string	m_selected;
	string	m_paraphrase;
	string	m_query_output;
};

#endif /*REGULUSDIALOGUERESULT_HPP_*/
