import React from 'react';
import io, { Socket } from 'socket.io-client';

import Canvas, { QueueInfo } from './Canvas';
import Pong, { GameInterface, GameState } from '../common/game/Pong';
import KeyPressMonitor from '../common/game/KeyPressMonitor';


interface ReactGameState {
	queue: QueueInfo
}

/*
This class is the GameClient.
There are possibly two types of clients: player or spectator.
Players can control their own game, spectators can only view the game.
*/
class Game extends React.Component <unknown, ReactGameState> {
	player: Set<number> = new Set<number>();
	socket?: Socket;
	keypress: KeyPressMonitor | null = null;
	game: GameInterface;
	latency = 0;
	
	constructor(props: unknown) {
		super(props);
		this.state = {
			queue: {
				position: Array(2).fill(-1),
				size: Array(2).fill(0)
			},
		};
		
		this.game = new Pong(400, 300, 2);
		this.socket = io("http://localhost:3001/game");  // change for production
		this.socket_handlers();	// initialise socket message handlers
		
		// calculate connection latency every 1 second
		setInterval(() => {
			const start = Date.now();
			// latency is assumed to be half of the time of the round trip
			this.socket?.emit("ping", () => {
				this.latency = (Date.now() - start) / 2;
			});
		}, 1000);
	}
	
	// define handler for socket messages
	socket_handlers() {
		// 'connect' event is fired upon connection the the Namespace
		this.socket?.on('connect', () => {
			console.log('Connected');
			this.getQueue();
			// keypress monitor is created if connection is made
			this.keypress = KeyPressMonitor.get_instance(
								this.onKeyDown.bind(this),
								this.onKeyUp.bind(this));
		});
		
		// server asks client to join a game
		this.socket?.on('join', (n: number) => {
			this.game.set_player(n);
			this.player.add(n);
		});
		
		// server asks client to unjoin a game
		this.socket?.on('unjoin', (n: number) => {
			console.log(`I quit as player ${n}`);
			this.socket?.emit('unjoin', n);
			this.player.delete(n);
			this.game.unset_player(n);
		});
		
		this.socket?.on('game_state', (game_state: GameState) => {
			this.game.set_state(this.latency, game_state);
		});
		
		this.socket?.on('exception', function(data) {
			console.log('event', data);
		});
		
		this.socket?.on('disconnect', () => {
			console.log('Disconnected');
			
			// keypress monitor is deleted if connection dropped
			if (this.keypress != null) {
				this.keypress = null;
			}
		});
		
		this.socket?.on("updateQueue", () => {
			this.getQueue()
		});
	}
	
	// callback function to be called by KeyPressMonitor class
	onKeyDown(key: KeyboardEvent["key"]): void {
		// do nothing if not a player
		if (this.player.size == 0)
			return ;
		// pass the current input to the game
		this.socket?.emit('keyDown', key);
		if (this.game != null && this.keypress != null)
			this.game.control(this.keypress.keypress);
	}
	
	// callback function to be called by KeyPressMonitor class
	onKeyUp(key: KeyboardEvent["key"]): void {
		// do nothing if not a player
		if (this.player.size == 0)
			return ;
		this.socket?.emit('keyUp', key);
		// pass the current input to the game
		if (this.game != null && this.keypress != null)
			this.game.control(this.keypress.keypress);
	}
	
	joinQuitClick(n: number): void {
		if (this.state.queue.position[n] == -1) {
			// if not in queue, let client join queue
			this.socket?.emit("join_queue", n, (n: number) => {
				console.log(`I am no. ${n + 1} in the queue`);
			});
		} else {
			// if not the current player, let client quit queue
			this.socket?.emit("unjoin_queue", n);
			console.log(`I've quit queue ${n}`);
		}
	}
	
	// get queue info from server
	getQueue() {
		this.socket?.emit('getQueue', (queue: QueueInfo) => {
			this.setState({
				queue: queue
			})
		});
	}
	
	deleteSocket(): void {
		delete this.socket;
	}
	
	render() {
		return <Canvas
			width={400}
			height={300}
			style={{border: "1px solid black"}}
			game={this.game}
			queue={this.state.queue}
			joinQuitClick={this.joinQuitClick.bind(this)}
			deleteSocket={this.deleteSocket.bind(this)}
			/>;
	}
}

export default Game;
