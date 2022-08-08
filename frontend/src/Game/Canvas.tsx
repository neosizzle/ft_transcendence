import React from 'react';

import { GameInterface } from '../common/game/Pong';
import './Canvas.css';


export function Button(props: any) {
	return (
		<button
			type="button"
			className="join_button"
			style={props.style}
			disabled={props.position == 0}
			onClick={props.onClick}>
				{props.position >= 0 ? props.position + 1 + " of " : ""}
				{props.size} in Queue.
				{props.position == 0 ? "" :
					<>
					<br></br>
					Click to {props.position >= 0 ? "un" : ""}join.
					</>
				}
		</button>
	);
}


interface CanvasProps {
	width: number,
	height: number,
	style: object,
	game: GameInterface,
	queue: {position: number[], size: number[]};
	joinQuitClick: (n: number) => void;
	deleteSocket: () => void;
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
						onClick={() => this.props.joinQuitClick(1)}
						position={this.props.queue.position[1]}
						size={this.props.queue.size[1]}
						/>
					<Button style={{float: "right"}}
						onClick={() => this.props.joinQuitClick(0)}
						position={this.props.queue.position[0]}
						size={this.props.queue.size[0]}
						/>
				</div>
			</div>
		);
	}
}
