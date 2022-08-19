import React from 'react';
import { Socket } from 'socket.io-client';

import Canvas, { QueueInfo } from './Canvas';
import Pong, { GameInterface, GameState } from '../common/game/Pong';
import KeyPressMonitor from '../common/game/KeyPressMonitor';
import { useAuth, AuthCtx } from '../context/authContext';


// this code is needed so as to get useAuth Hook to work with Game class
/* eslint-disable @typescript-eslint/no-explicit-any */
const useAuthHOC = (Component: any) => {
	// eslint-disable-next-line react/display-name
	return (props: any) => {
		const auth = useAuth();
		
		return <Component auth={auth} {...props} />;
	};
};
/* eslint-enable @typescript-eslint/no-explicit-any */

interface authProps {
	auth: AuthCtx | null
}

interface ReactGameState {
	queue: QueueInfo
	gameType: boolean
	gameState: GameState
}

/*
This class is the GameClient.
There are possibly two types of clients: player or spectator.
Players can control their own game, spectators can only view the game.
*/
class Game extends React.Component <authProps, ReactGameState> {
	player: Set<number> = new Set<number>();
	socket?: Socket | null;
	keypress: KeyPressMonitor | null = null;
	game: GameInterface;
	latency = 0;
	
	constructor(props: authProps) {
		super(props);
		this.state = {
			queue: {
				position: Array(2).fill(-1),
				size: Array(2).fill(0)
			},
			gameType: false,
			gameState: {
				isRunning: [],
				score: [],
				entity: [],
			},
		};
		
		this.game = new Pong(400, 300);
		this.socket = props.auth?.gameSocket;
		this.socket_handlers();	// initialise socket message handlers
		this.getQueue();
		this.getGameType();
		this.keypress = KeyPressMonitor.get_instance(
							this.onKeyDown.bind(this),
							this.onKeyUp.bind(this));
		
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
		});
		
		// server asks client to join a game
		this.socket?.on('join', (n: number) => {
			this.game.set_player(n);
			this.player.add(n);
		});
		
		// server asks client to unjoin a game
		this.socket?.on('unjoin', (n: number) => {
			console.log(`I quit as player ${n}`);
			this.player.delete(n);
			this.game.unset_player(n);
		});
		
		this.socket?.on('game_state', (game_state: GameState) => {
			this.game.set_state(this.latency, game_state);
			this.setState({gameState: game_state});
		});
		
		this.socket?.on('game_type', (type: boolean) => {
			this.game.set_type(type);
			this.setState({gameType: type});
		})
		
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
		this.socket?.disconnect();
		delete this.socket;
	}
	
	setGameType(type: boolean) {
		this.setState({gameType: type});
		this.socket?.emit('set_game_type', type);
	}
	
	// get game type from server
	getGameType() {
		this.socket?.emit("get_game_type", (type: boolean) => {
			this.setState({
				gameType: type,
			})
			this.game.type = type;
		});
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
			setGameType={this.setGameType.bind(this)}
			gameType={this.state.gameType}
			gameState={this.state.gameState}
			/>;
	}
}

export default useAuthHOC(Game);
