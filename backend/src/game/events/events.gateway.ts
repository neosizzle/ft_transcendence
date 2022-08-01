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
import { KeyPressMonitorBase } from '../../common/game/KeyPressMonitor'


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
	players: Map<string, number> = new Map();
	private keys: Map<number, string[]> = new Map([
		[0, ["ArrowUp", "ArrowDown"]],
		[1, ["w", "s"]],
		[2, ["ArrowLeft", "ArrowRight"]],
		[3, ["a", "d"]],
	]);
	
	// records a connected client
	handleConnection(client: Socket): void {
		if (this.players.size >= 2)
			return ;
		// record player and send back the player number
		let n: number
		if (this.players.size == 0)
			n = 0;
		else {
			for (const player of this.players.entries())
			{
				n = (player[1] + 1) % 2;
				break ;
			}
		}
		this.players.set(client.id, n);
		console.log(`user ${client.id} connected`);
		client.emit('identity', n);
	}
	
	// records an unconnected player
	handleDisconnect(client: Socket): void {
		const player = this.players.get(client.id);
		if (player === undefined)
			return ;
		console.log(`player ${player} disconnected`);
		// Assume that the disconnected player no longer presses the key
		for (const k of this.keys.get(player))
			KeyPressMonitorBase.delete(k);
		this.players.delete(client.id);
	}
	
	// records a keydown event from players only
	@SubscribeMessage('keydown')
	keyDown(@MessageBody() key: string, @ConnectedSocket() client: Socket)
			: void {
		const player = this.players.get(client.id);
		if (player === undefined || this.keys.get(player).indexOf(key) < 0) {
			return ;
		}
		console.log(`Received keydown ${key} from ${client.id}`);
		KeyPressMonitorBase.add(key);
	}
	
	// records a keyup event from players only
	@SubscribeMessage('keyup')
	keyUp(@MessageBody() key: string, @ConnectedSocket() client: Socket)
			: void {
		const player = this.players.get(client.id);
		if (player === undefined || this.keys.get(player).indexOf(key) < 0)
			return ;
		console.log(`Received keyup ${key} from ${client.id}`);
		KeyPressMonitorBase.delete(key);
	}
}
