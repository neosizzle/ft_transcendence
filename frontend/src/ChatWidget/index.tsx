import React, { FunctionComponent, useEffect, useRef, useState } from "react";
import { useNavigate } from "react-router-dom";
import {
  BaseWSResponse,
  Message,
  Room,
  SocketInterface,
} from "../Chat/classes";
import { useAuth } from "../context/authContext";
import { useChatWidget } from "../context/chatWidgetContext";
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
  const [openWindow, setOpenWindow] = useState<boolean>(false); // open chat window
  const auth = useAuth();
  const widget = useChatWidget();
  const navigate = useNavigate();

  // handle chat window close
  const handleClose = (e: MouseEvent) => {
    const id: string = (e.target as Element).id;
    const parentChatWin = (e.target as Element).closest("#chat-win");
    if (id !== "toggle-chat-win-btn" && !(id === "chat-win" || parentChatWin)) {
      setOpenWindow(false);
    }
  };

  // WS ACTIONS
  /**
   * Incoming new message
   *
   * 1. Check if message is current active room. If yes, append message to list of messages
   * 2. If no, check of roomid is in list. If yes, enable notification at roomId
   * 3. If no, append room to roomlist and append message
   * @param data data from ws server
   */
  const handleNewMsg = (data: Message) => {
    console.log("data ", data.room, " widget ", widget?.currActiveRoomRef.current)
    // if (data.roomId === widget?.currActiveRoom?.id) {
    //   const newMessages = [...(widget?.activeRoomMessages as Message[])];
    //   newMessages.shift();
    //   newMessages.push();
    //   widget?.setActiveRoomMessages(newMessages);
    //   //update last msg
    // } else if (
    //   widget?.rooms?.filter((e) => e.id === data.roomId) &&
    //   widget?.rooms?.filter((e) => e.id === data.roomId).length > 0
    // ) {
    //   const newNotify = [...(widget.notify as number[])];
    //   newNotify.push(data.roomId);
    //   widget.setNotify(newNotify);
    //   //update last msg
    // } else {
    //   const rooms = [...(widget?.rooms as Room[])];
    //   const lastMsgs = [...(widget?.lastMessages as Message[])];

    //   rooms.push(data.room);
    //   lastMsgs.push(data);
    // }
  };

  /**
   * Incoming error event
   *
   * 1/ Redirect to logout if error is forbidden
   * 2. Display error popup / dialog
   * @param data data from ws server
   */
  const handleError = (data: BaseWSResponse) => {
    if (data.message === "Forbidden") navigate("/logout");
    else {
      if (data.message)  widget?.setAlertMessage(data.message);
      widget?.setOpenAlert({ type: "error", isOpen: true });
    }
  };

  /**
   * Handle owner change
   *
   * 1. If owner is not curr user, remove user owner status
   * 2. Add owner indication on new owner
   */
  const handleOwnerChange = (data: BaseWSResponse) => {
    void data;
    console.info("widget does not respond to owner change");
  };

  /**
   * Handle Kick
   *
   * 1. If kicked user is self, leave room
   * 2. If not self, update member list
   */
  const handleKick = (data: BaseWSResponse) => {
    console.log(data);
    alert("KICKED " + data.message);
  };

  /**
   * Handle Ban
   *
   * 1. If banned user is self, leave room
   * 2. If not self, update member list
   */
  const handleBan = (data: BaseWSResponse) => {
    console.log(data);
    alert("BANNED " + data.message);
  };

  /**
   * Handle Promotion
   *
   * 1. If self is promoted, enable admin privelleges
   */
  const handlePromotion = (data: BaseWSResponse) => {
    console.log(data);
    alert("PROMOTED " + data.message);
  };

  /**
   * Handle Demotion
   *
   * 1. If self is demoted, enable admin privelleges
   */
  const handleDemotion = (data: BaseWSResponse) => {
    console.log(data);
    alert("DEMOTED " + data.message);
  };

  // initial actions
  useEffect(() => {
    // initialize socket
    // const socket = new SocketInterface(
    //   handleNewMsg,
    //   handleError,
    //   handleOwnerChange,
    //   handleKick,
    //   handleBan,
    //   handlePromotion,
    //   handleDemotion
    // );

    // widget?.setSocket(socket);

    console.log(auth?.chatSocket);
    if (!auth?.chatSocket) return;
    auth?.chatSocket?.on('newMessage', handleNewMsg)

    // add document listener
    document.addEventListener("click", handleClose);

    return () => {
      document.removeEventListener("click", handleClose);
    };
  }, [auth]);

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
      {widget?.notify ? (
        <span className="top-0 left-7 absolute w-3.5 h-3.5 bg-green-400 border-2 dark:border-gray-800 rounded-full lg:left-1.5 lg:top-2.5"></span>
      ) : null}

      {/* Window */}
      <ChatWindow open={openWindow} />
    </div>
  );
};

export default ChatWidget;
