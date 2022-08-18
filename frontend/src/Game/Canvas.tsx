import React from 'react';

import { GameInterface } from '../common/game/Pong';
import './Canvas.css';


export type QueueInfo =  {
	position: number[],
	size: number[]
}

type ButtonProps = {
	style: object,
	onClick: React.MouseEventHandler<HTMLButtonElement>,
	queue: QueueInfo,
	queue_no: number
}

// Button for joining or quiting game queue
function Button(props: ButtonProps) {
	const pos_this = props.queue.position[props.queue_no];
	const size = props.queue.size[props.queue_no];
	const pos_other = props.queue.position[(props.queue_no + 1) % 2];
	const disabled = pos_this == 0 || pos_other >= 0;
	
	return (
		<button
			type="button"
			className="join_button"
			style={props.style}
			disabled={disabled}
			onClick={props.onClick}>
				{pos_this >= 0 ? pos_this + 1 + " of " : ""}
				{size} in Queue.
				{pos_this == 0 ? "" :
					<>
					<br></br>
					{disabled ? "" :
						<>
						Click to {pos_this >= 0 ? "un" : ""}join.
						</>
					}
					</>
				}
		</button>
	);
}

type ToggleButtonProps = {
	style: object,
	onClick: React.MouseEventHandler<HTMLButtonElement>,
	queue: QueueInfo,
	gameType: boolean,
}

// Button to toggle between Original or Customised pong game
function ToggleButton(props: ToggleButtonProps) {
	const pos_this = props.queue.position[0];
	const disabled = pos_this != 0;
	
	return (
		<button
			type="button"
			className="join_button"
			style={props.style}
			disabled={disabled}
			onClick={props.onClick}
		>
			{props.gameType ? "Customised" : "Original"}
		</button>
	)
}


interface CanvasProps {
	width: number,
	height: number,
	style: object,
	game: GameInterface,
	queue: {position: number[], size: number[]};
	joinQuitClick: (n: number) => void;
	deleteSocket: () => void;
	setGameType: (type: boolean) => void;
	gameType: boolean
}


export default class Canvas extends React.Component<CanvasProps> {
	// map to remember the status of a key
	canvasRef: React.RefObject<HTMLCanvasElement>;
	canvas: HTMLCanvasElement | null = null;
	ctx: CanvasRenderingContext2D | null = null;
	game: GameInterface;
	animationID = 0;
	
	constructor(props: CanvasProps) {
		super(props);
		this.game = props.game;
		this.canvasRef = React.createRef();
	}
	
	// initialise canvas, context and games once it is component is mounted
	componentDidMount() {
		if (this.canvasRef === null)
			return ;
			
		this.canvas = this.canvasRef.current;
		if (this.canvas === null)
			throw new Error("Canvas cannot be null");
		
		this.ctx = this.canvas.getContext('2d');
		if (this.ctx === null)
			throw new Error("Canvas context cannot be null");
		
		// default text settings
		this.ctx.textBaseline = "middle";
		this.ctx.textAlign = "center";
		this.ctx.font = "30px Arial";
		
		// run the animation loop
		this.animationID = window.requestAnimationFrame(this.update.bind(this));
	}
	
	// main game loop
	update() {
		this.game.update();	// update to the next frame
		this.game.draw(this.ctx);	// draw game state to canvas
		this.animationID = window.requestAnimationFrame(this.update.bind(this));
	}
	
	// actions to be taken when canvas is unmounted
	componentWillUnmount() {
		// cancel animation frame to prevent leak
		window.cancelAnimationFrame(this.animationID);
		// remove old socket
		this.props.deleteSocket();
	}
	
	render() {
		return (
			<div className="game_div">
				<div className="canvas_div">
				<canvas
					ref={this.canvasRef}
					width={this.props.width}
					height={this.props.height}
					style={this.props.style}
				/>
				</div>
				<div className="button_div">
					<Button style={{float: "left"}}
						onClick={() => this.props.joinQuitClick(0)}
						queue={this.props.queue}
						queue_no={0}
						/>
					
					<ToggleButton style={{float: "left"}}
						onClick={() => this.props.setGameType(!this.props.gameType)}
						queue={this.props.queue}
						gameType={this.props.gameType}
						/>
						
					<Button style={{float: "right"}}
						onClick={() => this.props.joinQuitClick(1)}
						queue={this.props.queue}
						queue_no={1}
						/>
				</div>
			</div>
		);
	}
}
