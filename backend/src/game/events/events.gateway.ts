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
	players: string[] = ["", ""];
	private keys: Map<number, string[]> = new Map([
		[0, ["ArrowUp", "ArrowDown"]],
		[1, ["w", "s"]],
		[2, ["ArrowLeft", "ArrowRight"]],
		[3, ["a", "d"]],
	]);
	
	// records a connected client
	handleConnection(client: Socket): void {
		console.log(`spectator ${client.id} connected`);
	}
	
	// records an unconnected player
	handleDisconnect(client: Socket): void {
		for (let player = 0; this.players.length; ++player)
		{
			if (this.players[player] != client.id)
				continue ;
			console.log(`player ${player} disconnected`);
			// Assume that the disconnected player no longer presses the key
			for (const k of this.keys.get(player))
				KeyPressMonitorBase.delete(k);
			this.players[player] = "";
		}
	}
	
	// attempts to join a game
	@SubscribeMessage('join')
	joinGame(@MessageBody() n: number, @ConnectedSocket() client: Socket)
			: number {
		console.log(this.players);
		
		// let player join if not occupied
		if (this.players[n] == "" || this.players[n] == client.id) {
			this.players[n] = client.id;
			return n;
		}
		else
			return -1;
	}
	
	// records a keydown event from players only
	@SubscribeMessage('keydown')
	keyDown(@MessageBody() key: string, @ConnectedSocket() client: Socket)
			: void {
		const player = this.players.indexOf(client.id);
		if (player < 0 || this.keys.get(player).indexOf(key) < 0) {
			return ;
		}
		console.log(`Received keydown ${key} from ${client.id}`);
		KeyPressMonitorBase.add(key);
	}
	
	// records a keyup event from players only
	@SubscribeMessage('keyup')
	keyUp(@MessageBody() key: string, @ConnectedSocket() client: Socket)
			: void {
		const player = this.players.indexOf(client.id);
		if (player < 0 || this.keys.get(player).indexOf(key) < 0)
			return ;
		console.log(`Received keyup ${key} from ${client.id}`);
		KeyPressMonitorBase.delete(key);
	}
}
