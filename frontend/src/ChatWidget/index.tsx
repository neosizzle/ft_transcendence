import { cloneDeep } from "lodash";
import React, { FunctionComponent, useEffect, useState } from "react";
import { useNavigate } from "react-router-dom";
import {
  Ban,
  BaseWSResponse,
  Member,
  Message,
  Room,
} from "../Chat/classes";
import { ERR, INCOMING_BAN, INCOMING_KICK, INCOMING_MSG } from "../constants";
import { useAuth } from "../context/authContext";
import { useChatWidget } from "../context/chatWidgetContext";
import ChatWindow from "./ChatWindow";

const PAGE_SIZE = 4;
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

  // WS ACTIONS -- Chat widget does not deal with admin promotion / demotion and owner transfer events
  /**
   * Incoming new message
   *
   * 1. Check if message is current active room. If yes, append message to list of messages
   * 2. If no, check of roomid is in list. If yes, enable notification at roomId
   * 3. If no, append room to roomlist and append message
   * @param data data from ws server
   */
  const handleNewMsg = (data: Message) => {
    // active room is where msg is sent
    if (data.roomId === widget?.currActiveRoomRef.current?.id) {
      const newMessages = cloneDeep(
        widget?.activeRoomMessagesRef.current as Message[]
      );
      newMessages.pop();
      newMessages.unshift(data);
      widget?.setActiveRoomMessages(newMessages);

      //update last msg
      const lastMessages = cloneDeep(
        widget.lastMessagesRef.current as Message[]
      );
      const lastMsgIdx = lastMessages.findIndex(
        (e) => e?.roomId === data.roomId
      );
      lastMessages[lastMsgIdx] = data;
      widget.setLastMessages(lastMessages);
    } else if (
      // user in room but its not active
      widget?.roomsRef.current?.filter((e) => e.id === data.roomId) &&
      widget?.roomsRef.current?.filter((e) => e.id === data.roomId).length > 0
    ) {
      //set notification
      const newNotify = cloneDeep(widget?.notifyRef.current as number[]) || [];
      newNotify.push(data.roomId);
      widget.setNotify(newNotify);

      // FEATURE can move notified room to top

      //update last msg
      const lastMessages = cloneDeep(
        widget.lastMessagesRef.current as Message[]
      );
      const lastMsgIdx = lastMessages.findIndex(
        (e) => e.roomId === data.roomId
      );
      lastMessages[lastMsgIdx] = data;
      widget.setLastMessages(lastMessages);
    } else {
      // user not in room
      // set notification
      const newNotify = cloneDeep(widget?.notifyRef.current as number[]) || [];

      newNotify.push(data.roomId);

      widget?.setNotify(newNotify);

      // add into roomlsit of roomlist has space
      if (
        widget?.roomsRef.current &&
        widget?.roomsRef.current?.length < PAGE_SIZE
      ) {
        const rooms = cloneDeep(widget?.roomsRef.current as Room[]);
        const lastMsgs = cloneDeep(
          widget?.lastMessagesRef.current as Message[]
        );

        rooms.push(data.room);
        lastMsgs.push(data);
        widget?.setRooms(rooms);
        widget?.setLastMessages(lastMsgs);
      }
    }
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
      if (data.message) widget?.setAlertMessage(data.message);
      widget?.setOpenAlert({ type: "error", isOpen: true });
    }
  };

  /**
   * Handle Kick
   *
   * 1. If kicked user is self, leave room
   * 2. If not self, update member list
   */
  const handleKick = (data: Member) => {
    // console.log(data);
    // alert("KICKED " + data.message);
    if (data.userId == auth?.user?.id) {
      const newRooms = widget?.roomsRef.current?.filter(
        (e) => e.id !== data.roomId
      );

      if (widget?.currActiveRoomRef.current?.id === data.roomId)
        widget?.setCurrActiveRoom(null);
      widget?.setRooms(newRooms as Room[] | null);
    }
  };

  /**
   * Handle Ban
   *
   * 1. If banned user is self, leave room
   * 2. If not self, update member list
   */
  const handleBan = (data: Ban) => {
    if (data.userId == auth?.user?.id) {
      const newRooms = widget?.roomsRef.current?.filter(
        (e) => e.id !== data.roomId
      );

      if (widget?.currActiveRoomRef.current?.id === data.roomId)
        widget?.setCurrActiveRoom(null);
      widget?.setRooms(newRooms as Room[] | null);
    }
    console.log(data);
    alert("BANNED ");
  };

  // initial actions
  useEffect(() => {

    // add socket listeners
    if (!auth?.chatSocket) return;
    auth.chatSocket.on(INCOMING_MSG, handleNewMsg);
    auth.chatSocket.on(ERR, handleError);
    auth.chatSocket.on(INCOMING_KICK, handleKick);
    auth.chatSocket.on(INCOMING_BAN, handleBan);

    // add document listener
    document.addEventListener("click", handleClose);

    return () => {
      // remove socket listeners
      auth?.chatSocket?.off(INCOMING_MSG, handleNewMsg);
      auth?.chatSocket?.off(ERR, handleError);
      auth?.chatSocket?.off(INCOMING_KICK, handleKick);
      auth?.chatSocket?.off(INCOMING_BAN, handleBan)

      // remove document listener
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
