import {
	OnGatewayConnection,
	OnGatewayDisconnect,
	MessageBody,
	SubscribeMessage,
	WebSocketGateway,
	WebSocketServer,
	} from '@nestjs/websockets';
import { Server, Socket } from 'socket.io';

@WebSocketGateway({
	cors: {
		origin: 'http://localhost:3000',	// change this for production
	},
	namespace: 'game',
})
export class GameEventsGateway
		implements OnGatewayConnection, OnGatewayDisconnect {
	@WebSocketServer()
	server: Server;
	
	connections: Map<number, Socket> = new Map();
	
	// records a connected client
	handleConnection(client: Socket): void {
		// disconnect client if there are already 2 connections
		if (this.connections.size == 2) {
			console.log(`denied connection from ${client.id}`);
			client.disconnect();
		} else {
			const n: number = this.connections.has(0) ? 1 : 0;
			this.connections.set(n, client);
			console.log(`user ${client.id} connected`);
			client.emit('identity', n);
		}
	}
	
	// records an unconnected client
	handleDisconnect(client: Socket): void {
		for (const entry of this.connections.entries())
			if (entry[1] == client)
				this.connections.delete(entry[0]);
		console.log(`user ${client.id} disconnected`);
	}
}
