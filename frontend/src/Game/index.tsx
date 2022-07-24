import React, { useRef, useEffect } from 'react';

const Game = (props: any) => {
	const canvasRef = useRef<HTMLCanvasElement>(null);
	
	// Effect Hook performs side effect in function components
	useEffect(() => {
		const canvas = canvasRef.current;
		if (canvas == null)
			return ;
		
		const context = canvas.getContext('2d');
		if (context == null)
			return ;
		}, []);
	
	return (
		<canvas
			ref={canvasRef}
			width="400"
			height="300"
			style={{border: "1px solid black"}}
			{...props}
		/>
	)
}

export default Game;
