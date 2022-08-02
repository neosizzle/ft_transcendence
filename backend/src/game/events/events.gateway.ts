import {
	OnGatewayConnection,
	OnGatewayDisconnect,
	OnGatewayInit,
	MessageBody,
	ConnectedSocket,
	SubscribeMessage,
	WebSocketGateway,
	WebSocketServer,
	} from '@nestjs/websockets';
import { Server, Socket } from 'socket.io';

import GameServer from '../server'


@WebSocketGateway({
	cors: {
		origin: 'http://localhost:3000',	// change this for production
	},
	namespace: 'game',
})
export class GameEventsGateway
		implements OnGatewayInit, OnGatewayConnection, OnGatewayDisconnect {
	@WebSocketServer()
	server: Server;
	clients: Set<Socket> = new Set();
	game_server: GameServer;
	
	afterInit(): void {
		this.game_server = new GameServer(this.server)
	}
	
	// records a connected client
	handleConnection(client: Socket): void {
		console.log(`spectator ${client.id} connected`);
		this.game_server.spectate(client);
		this.clients.add(client);
	}
	
	// records an unconnected client / player
	handleDisconnect(client: Socket): void {
		console.log(`spectator ${client.id} disconnected`);
		this.clients.delete(client);
		this.game_server.disconnect(client.id);
	}
	
	// attempts to join a game
	@SubscribeMessage('join')
	joinGame(@MessageBody() n: number, @ConnectedSocket() client: Socket)
			: number {
		return this.game_server.connect(client.id, n);
	}
	
	// records a keydown event from players only
	@SubscribeMessage('keyDown')
	keyDown(@MessageBody() key: string, @ConnectedSocket() client: Socket)
			: void {
		if (key == ' ')
			this.game_server.start(client.id);
		this.game_server.keyDown(client.id, key);
	}
	
	// records a keyup event from players only
	@SubscribeMessage('keyUp')
	keyUp(@MessageBody() key: string, @ConnectedSocket() client: Socket)
			: void {
		this.game_server.keyUp(client.id, key);
	}
}
