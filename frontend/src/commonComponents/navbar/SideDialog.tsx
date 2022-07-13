import React, { FunctionComponent, useEffect, useState } from "react";
import { Link } from "react-router-dom";

interface SideDialogProps {
	setOpenMenu : React.Dispatch<React.SetStateAction<boolean>>;
}
 
const SideDialog: FunctionComponent<SideDialogProps> = ({setOpenMenu}) => {
	const [width, setWidth] = useState("0")

	useEffect(() => {
		setWidth("1/2");
	}, [])
	
	return ( 
		<div
              onClick={(e)=>{
                if(e.currentTarget != e.target ) return;
                setOpenMenu(false)
              }}
              className='
              fixed
              z-10
              left-0
              top-0
              w-full
              h-full
              bg-gray-500/50
              '
              >
                {/* Menu content */}
                <div className={`
                bg-white
                h-full
                w-${width}
                z-20
				duration-300
				transform
				transition-all
				ease-out
				`}
				
				>

                  {/* X button */}
                  <div className='
                  flex
                  justify-end
                  '>
                    <button onClick={()=>setOpenMenu(false)} className="mt-4 mr-6">
                    <svg xmlns="http://www.w3.org/2000/svg" className="h-6 w-6" fill="none" viewBox="0 0 24 24" stroke="currentColor" strokeWidth={2}>
                      <path strokeLinecap="round" strokeLinejoin="round" d="M6 18L18 6M6 6l12 12" />
                    </svg>
                    </button>
                  </div>

                  {/* Links */}
                  <div className="grid grid-rows-6 grid-flow-col gap-4 justify-center items-center">
                    <div onClick={()=>setOpenMenu(false)}>
                      <Link to="/" className="">
                      <img className="block h-10 w-auto" src="/assets/logo/42logo.png" alt="Workflow" />
                      </Link>
                    </div>
                    <div onClick={()=>setOpenMenu(false)} className='w-full'><Link to="/chat" className="block px-6 py-2 text-sm font-medium">Chat</Link></div>
                    <div onClick={()=>setOpenMenu(false)} className='w-full'><Link to="/users/profile/me" className="block px-6 py-2 text-sm font-medium">Users</Link></div>
                    <div onClick={()=>setOpenMenu(false)} className='w-full'><Link to="/game" className="block px-6 py-2 text-sm font-medium">Game</Link></div>
                    
                  </div>
                </div>
              </div>
	);
}
 
export default SideDialog;