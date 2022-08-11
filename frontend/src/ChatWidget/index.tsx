import React, { FunctionComponent, useEffect, useState } from "react";
import { useAuth } from "../context/authContext";
import ChatWindow from "./ChatWindow";

const ChatIcon: FunctionComponent = () => {
  return (
    <svg
      id="toggle-chat-win-btn"
      xmlns="http://www.w3.org/2000/svg"
      className="h-4 w-4 text-white"
      fill="none"
      viewBox="0 0 24 24"
      stroke="currentColor"
      strokeWidth={2}
    >
      <path
        strokeLinecap="round"
        strokeLinejoin="round"
        d="M8 10h.01M12 10h.01M16 10h.01M9 16H5a2 2 0 01-2-2V6a2 2 0 012-2h14a2 2 0 012 2v8a2 2 0 01-2 2h-5l-5 5v-5z"
      />
    </svg>
  );
};

const ChatWidget: FunctionComponent = () => {
  const [notify, setNotify] = useState<boolean>(false); // notofication indicator
  const [openWindow, setOpenWindow] = useState<boolean>(false); // open chat window
  const auth = useAuth();

  const handleClose = (e: MouseEvent) => {
    const id: string = (e.target as Element).id;
    const parentChatWin = (e.target as Element).closest("#chat-win");
    if (id !== "toggle-chat-win-btn" && !(id === "chat-win" || parentChatWin)) {
      // console.log("closing window ", parentChatWin)
      setOpenWindow(false);
    }
  };

  // initial actions
  useEffect(() => {
    // add document listener
    document.addEventListener("click", handleClose);

    return () => {
      document.removeEventListener("click", handleClose);
    };
  }, []);

  return !auth?.user ? null : (
    <div
      className={`fixed border border-slate-600 z-10 right-10 bottom-10 h-10 w-10 flex justify-center items-center bg-black rounded-full lg:right-5 lg:bottom-0 lg:rounded-none lg:h-10 lg:w-80 lg:justify-start`}
    >
      {/* Icon btn */}
      <button
        onClick={() => setOpenWindow(!openWindow)}
        className="lg:hidden p-1"
      >
        <ChatIcon />
      </button>

      {/* Tab btn */}
      <button
        id="toggle-chat-win-btn"
        onClick={() => setOpenWindow(!openWindow)}
        className="hidden lg:block w-full text-left text-white px-5"
      >
        Messages
      </button>

      {/* Notification indicator */}
      {notify ? (
        <span className="top-0 left-7 absolute w-3.5 h-3.5 bg-green-400 border-2 dark:border-gray-800 rounded-full lg:left-1.5 lg:top-2.5"></span>
      ) : null}

      {/* Window */}
      <ChatWindow open={openWindow}/>
    </div>
  );
};

export default ChatWidget;
