import {
	OnGatewayConnection,
	OnGatewayDisconnect,
	MessageBody,
	ConnectedSocket,
	SubscribeMessage,
	WebSocketGateway,
	WebSocketServer,
	} from '@nestjs/websockets';
import { Server, Socket } from 'socket.io';
import KeyPressMonitor from '../../common/game/KeyPressMonitor'


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
	keypress: KeyPressMonitor = KeyPressMonitor.get_instance();
	connections: Map<string, number> = new Map();
	
	private player = -1;
	private valid_key: Map<number, string[]> = new Map([
		[0, ["Arrow Up", "Arrow Down"]],
		[1, ["w", "s"]],
		[2, ["Arrow Left", "Arrow Right"]],
		[3, ["a", "d"]],
	]);
	
	// records a connected client
	handleConnection(client: Socket): void {
		// disconnect client if there are already 2 connections
		if (this.connections.size == 2) {
			console.log(`denied connection from ${client.id}`);
			client.disconnect();
		} else {
			// record player and send back the player number
			let n: number
			if (this.connections.size == 0)
				n = 0;
			else {
				for (const player of this.connections.entries())
				{
					n = (player[1] + 1) % 2;
					break ;
				}
			}
			this.connections.set(client.id, n);
			console.log(`user ${client.id} connected`);
			client.emit('identity', n);
		}
	}
	
	// records an unconnected client
	handleDisconnect(client: Socket): void {
		console.log(`user ${this.connections.get(client.id)} disconnected`);
		this.connections.delete(client.id);
	}
	
	// records a keydown event
	@SubscribeMessage('keydown')
	keyDown(@MessageBody() data: string, @ConnectedSocket() client: Socket)
			: void {
		console.log(`Received keydown ${data} from ${client.id}`);
		KeyPressMonitor.add(data);
	}
	
	// records a keyup event
	@SubscribeMessage('keyup')
	keyUp(@MessageBody() data: string, @ConnectedSocket() client: Socket)
			: void {
		console.log(`Received keyup ${data} from ${client.id}`);
		KeyPressMonitor.delete(data);
	}
}
