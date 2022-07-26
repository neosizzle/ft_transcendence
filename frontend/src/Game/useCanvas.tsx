import { useRef, useEffect } from 'react'

const useCanvas = () => {
	const canvasRef = useRef<HTMLCanvasElement>(null);
	
	useEffect(() => {
		const canvas = canvasRef.current
		if (canvas == null)
			return ;
		
		const context = canvas.getContext('2d')
		if (context == null)
			return ;
		
		let animationFrameId: number;
		
		const render = () => {
			animationFrameId = window.requestAnimationFrame(render)
		}
		render()
		
		return () => {
			window.cancelAnimationFrame(animationFrameId)
		}
	}, []);
	
	return canvasRef;
}

export default useCanvas;