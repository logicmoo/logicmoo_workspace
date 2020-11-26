/*
 * Copyright 2007-2009 TIM/ETI University of Geneva.
 * All Rights Reserved. Use is subject to license terms.
 *
 * File: 	SocketsEngine.hpp
 * Author:	Nikos Tsourakis <Nikolaos.Tsourakis@unige.ch>
 *
 * See the file "license.terms" for information on usage and
 * redistribution of this file, and for a DISCLAIMER OF ALL
 * WARRANTIES.
 */
 
#ifndef SOCKETSENGINE_HPP_
#define SOCKETSENGINE_HPP_

#include <sstream>
#include "SocketObserver.hpp"
#include "SocketsEngineObserver.hpp"
#include "SocketServer.hpp"
#include "SocketRead.hpp"
#include "SocketWrite.hpp"
#include "SocketClass.hpp"

using namespace std;

class SocketsEngine : public SocketObserver
{
public:
	
	SocketsEngine(SocketsEngineObserver& observer);
	~SocketsEngine();
	
	// Sets server name
	void SetServerName(const string name);
	// Gets mrcp server name
	string GetServerName() const;
	// Sets server port
	void SetServerPort(Uint32 port);
	// Gets server port
	Uint32 GetServerPort() const;
	// Set the size of the data that must be read
	void SetBufferSize(Uint16 length);
	// Get the size of the data that must be read
	Uint16 GetBufferSize() const;
	// Set the local port (for udp connections)
	void SetLocalPort(Uint32 port);
	// Get the local port (for udp connections)
	Uint32 GetLocalPort(void) const;
	
	// Connect to server
	void ConnectServer(string type);
	// Disconnect from server
	void DisconnectServer();
	// Run engine as server
	void RunAsServer();
		
	// Writes data to socket
	Uint16 WriteData(const Uint8* buffer, Uint16 len);
	
	// Read data from socket
	void ReadData();
	
	// From SocketObserver 
	// Packet received from socket
	void PacketReceived(ostringstream& buffer);
	
	// From SocketObserver
	// New client arrived
	void ClientConnected(TcpSocket* socket);

private:
	
	// Avoid accidental copy or assignment
	SocketsEngine(const SocketsEngine&);
	SocketsEngine& operator = (const SocketsEngine&);
	
private:

	// Initialize the Socket Engine
	void Initialize();
	
	SocketsEngineObserver&	m_observer;
	SocketServer*			m_socket_server;
	SocketRead*				m_socket_read;
	SocketWrite*			m_socket_write;
	SocketClass*			m_socket;	
	string					m_server_name;
	int						m_server_port;
	int						m_local_port;	// Used for udp connections
	Uint16					m_buffer_size;
};

#endif /*SOCKETSENGINE_HPP_*/
