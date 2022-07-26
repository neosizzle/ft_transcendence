import React from 'react';
import useCanvas from './useCanvas'
import { KeyPressMonitor } from './KeyPressMonitor'
import { Pong } from './Pong'

interface GameProps {
	width: number,
	height: number,
	style: object
}


const Game = (props: GameProps) => {
	const keypress: KeyPressMonitor = KeyPressMonitor.get_instance();
	const canvasRef = useCanvas();
	// console.log(canvasRef);
	const pong: Pong = new Pong(canvasRef.current, 2);
	
	return <canvas ref={canvasRef} {...props}/>;
}

export default Game;
