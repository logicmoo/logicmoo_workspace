#include <iostream>
#include <fstream>
#include <string>
#include "pb-vector.pb.h"

using namespace std;

class Exception
{
private:
		string data;
public:
		Exception(const string why) {
			data = why;
		}

		const string reason() {
			return data;
		}
};

/*
 * Abstract class
 */

class DoublePBVector;

class PBVector
{
protected:
	Vector numberList;

private:
	int serializeToStream(ostream &stream)
		{
		return numberList.SerializeToOstream(&stream);
		}

	int parseFromStream(istream &stream)
		{
		return numberList.ParseFromIstream(&stream);
		}

	int serializeToString(string & data)
		{
		return numberList.SerializeToString( &data);
		}

	int parseFromString(const string &data)
		{
		return numberList.ParseFromString(data);
		}

public:
	friend ostream & operator << (ostream & out, PBVector & a)
		{
		int ret = a.serializeToStream(out);

		if(!ret)
			throw Exception("error while serializing output stream!");

		return out;
		}

	friend string & operator << (string & out, PBVector & a)
		{
		int ret = a.serializeToString(out);

		if(!ret)
			throw Exception("error while serializing output string!");

		return out;
		}

	friend PBVector & operator << (PBVector & a, istream & cin)
		{
		int ret = a.parseFromStream(cin);
		
		if(!ret)
			throw Exception("error while parsing input stream!");

		return a;
		}

	friend PBVector & operator << (PBVector & a, string & cin)
		{
		int ret = a.parseFromString(cin);
		
		if(!ret)
			throw Exception("error while parsing input string!");

		return a;
		}
};

/*
 * Concrete classes
 */

class DoublePBVector : public PBVector
{

public: 
	void add_value(double value) 
		{
		numberList.add_double_values(value);
		}

	void value(int at_index, double value)
		{
		numberList.set_double_values(at_index, value);
		}

	double value(int at_index)
		{
		return numberList.double_values(at_index);
		}

	int size(void)
		{
		return numberList.double_values_size();
		}
}; 

class FloatPBVector : public PBVector
{
public: 
	void add_value(float value) 
		{
		numberList.add_float_values(value);
		}

	void value(int at_index, float value)
		{
		numberList.set_float_values(at_index, value);
		}

	float value(int at_index)
		{
		return numberList.float_values(at_index);
		}

	int size(void)
		{
		return numberList.float_values_size();
		}
};

class IntegerPBVector : public PBVector
{
public: 
	void add_value(int value) 
		{
		numberList.add_integer_values(value);
		}

	void value(int at_index, int value)
		{
		numberList.set_integer_values(at_index, value);
		}

	int value(int at_index)
		{
		return numberList.integer_values(at_index);
		}

	int size(void)
		{
		return numberList.integer_values_size();
		}
};

class Integer32PBVector : public PBVector
{
public: 
	void add_value(int32_t value) 
		{
		numberList.add_integer32_values(value);
		}

	void value(int at_index, int32_t value)
		{
		numberList.set_integer32_values(at_index, value);
		}

	int32_t value(int at_index)
		{
		return numberList.integer32_values(at_index);
		}

	int size(void)
		{
		return numberList.integer32_values_size();
		}
};

class Integer64PBVector : public PBVector
{
public: 
	void add_value(int64_t value) 
		{
		numberList.add_integer64_values(value);
		}

	void value(int at_index, int64_t value)
		{
		numberList.set_integer64_values(at_index, value);
		}

	int64_t value(int at_index)
		{
		return numberList.integer64_values(at_index);
		}

	int size(void)
		{
		return numberList.integer64_values_size();
		}
};

class UnsignedPBVector : public PBVector
{
public: 
	void add_value(unsigned value) 
		{
		numberList.add_unsigned_values(value);
		}

	void value(int at_index, unsigned value)
		{
		numberList.set_unsigned_values(at_index, value);
		}

	unsigned value(int at_index)
		{
		return numberList.unsigned_values(at_index);
		}

	int size(void)
		{
		return numberList.unsigned_values_size();
		}
};


class AtomPBVector : public PBVector
{
public: 
	void add_value(string value) 
		{
		numberList.add_atom_values(value);
		}

	void value(int at_index, string value)
		{
		numberList.set_string_values(at_index, value);
		}

	string value(int at_index)
		{
		return numberList.atom_values(at_index);
		}

	int size(void)
		{
		return numberList.atom_values_size();
		}
};

class StringPBVector : public PBVector
{
public: 
	void add_value(string value) 
		{
		numberList.add_string_values(value);
		}

	void value(int at_index, string value)
		{
		numberList.set_string_values(at_index, value);
		}

	string value(int at_index)
		{
		return numberList.string_values(at_index);
		}

	int size(void)
		{
		return numberList.string_values_size();
		}
};


	string stuff[] = { "one", "two", "three", "four", "five", };

int main(int argc, char *argv[])
{
	GOOGLE_PROTOBUF_VERIFY_VERSION;

	DoublePBVector buffer, buffer1;

	register int i;

	try 
		{
		buffer << cin;
		} 

	catch (Exception e) 
		{
		cerr << "exception: " << e.reason() << "\n";

		exit(1);
		}

	string serialized_data;

	serialized_data << buffer;

	buffer1 << serialized_data;

	int size = buffer1.size();

	for(i = 0; i < size; i++)
		cout << buffer1.value(i) << "\n";

	return 0;
}
