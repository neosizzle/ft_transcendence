import { UseGuards } from "@nestjs/common";
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
import { UsersService } from 'src/users/users/users.service';
import { MatchService } from "src/match/match.service";
import { Server, Socket } from 'socket.io';
import GameServer, { QueueInfo } from '../server'
import { AuthGuard } from "src/users/auth/guard";


@UseGuards(AuthGuard)
@WebSocketGateway({
	cors: {
		origin: '*',	// change this for production
	},
	namespace: 'game',
})
export class GameEventsGateway
		implements OnGatewayInit, OnGatewayConnection, OnGatewayDisconnect {
	@WebSocketServer()
	server: Server;
	clients: Set<Socket> = new Set();
	game_server: GameServer;
	
	constructor(
		private usersService: UsersService,
		private matchService: MatchService
	) {}
	
	afterInit(): void {
		this.game_server = new GameServer(
			this.server, this.usersService, this.matchService);
	}
	
	// records a connected client. This happens whenever a socket connection
	// is established between client and server.
	handleConnection(client: Socket): void {
		// console.log(`spectator ${client.id} connected`);
		this.game_server.spectate(client);
		this.clients.add(client);
	}
	
	// records an unconnected client / player. This happens whenever a socket
	// connection is disconnected.
	handleDisconnect(client: Socket): void {
		// console.log(`spectator ${client.id} disconnected`);
		this.clients.delete(client);
		this.game_server.handleDisconnect(client);
	}
	
	// ping message. Clients pings server at regular interval to calculate
	// the latency between server and client.
	@SubscribeMessage('ping')
	pingResponse(): boolean {
		return true;
	}
	
	// client requests to join queue 'n'
	@SubscribeMessage("join_queue")
	joinGame(@MessageBody() n: number, @ConnectedSocket() client: Socket)
			: number {
		return this.game_server.handleConnect(client, n);
	}
	
	// client requests to unjoins queue 'n'
	@SubscribeMessage("unjoin_queue")
	unjoinGame(@MessageBody() n: number, @ConnectedSocket() client: Socket)
			: void {
		this.game_server.handleDisconnect(client, n);
	}
	
	// client requests to change game type
	@SubscribeMessage("set_game_type")
	setGameType(@MessageBody() type: boolean)
	: void {
		this.game_server.setGameType(type)
	}
	
	@SubscribeMessage("get_game_type")
	getGameType(@ConnectedSocket() client: Socket) {
		client.emit("game_type", this.game_server.game.type);
	}
	
	// records a keydown event from players only
	@SubscribeMessage('keyDown')
	keyDown(@MessageBody() key: string, @ConnectedSocket() client: Socket)
			: void {
		if (key == ' ')
			this.game_server.start(client);
		this.game_server.keyDown(client, key);
	}
	
	// records a keyup event from players only
	@SubscribeMessage('keyUp')
	keyUp(@MessageBody() key: string, @ConnectedSocket() client: Socket)
			: void {
		this.game_server.keyUp(client, key);
	}
	
	// client requests for the queue data from the server
	@SubscribeMessage('getQueue')
	getQueue(@ConnectedSocket() client: Socket): QueueInfo {
		return this.game_server.getQueue(client);
	}
	
	// client asks whether a user is in the queue. Returns true if user
	// is in queue, false otherwise.
	@SubscribeMessage('user_in_queue')
	userInQueue(@MessageBody() id: number): boolean {
		return this.game_server.userInQueue(id);
	}
}
