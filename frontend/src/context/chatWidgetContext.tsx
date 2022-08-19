import React, { createContext, MutableRefObject, useContext, useRef, useState } from "react";
import { Message, Room } from "../Chat/classes";
import { AlertType } from "../commonComponents/Alert";

interface Props {
  children: React.ReactNode;
}

export interface ChatWidgetCtx {
  currActiveRoom: Room | null;
  currActiveRoomRef : MutableRefObject<Room | null>;
  setCurrActiveRoom: (data : Room | null) => void;

  activeRoomMessages: Message[] | null;
  activeRoomMessagesRef : MutableRefObject <Message[] | null>;
  setActiveRoomMessages: (data : Message[] | null) => void;

  rooms: Room[] | null;
  roomsRef : MutableRefObject <Room[] | null>;
  setRooms: (data : Room[] | null) => void;

  notify: number[] | null;
  notifyRef : MutableRefObject<number[] | null>;
  setNotify: (data : number[] | null) => void;

  lastMessages: Message[] | null;
  lastMessagesRef : MutableRefObject <Message[] | null>;
  setLastMessages: (data : Message[] | null) => void;

  openAlert: AlertType;
  setOpenAlert: React.Dispatch<React.SetStateAction<AlertType>>;

  alertMessage : string;
  setAlertMessage : React.Dispatch<React.SetStateAction<string>>;
}

const ChatWidgetContext = createContext<ChatWidgetCtx | null>(null);

export const ChatWidgetProvider = (props: Props) => {
  const [currActiveRoom, _setCurrActiveRoom] = useState<Room | null>(null); // active viewing room
  const currActiveRoomRef = useRef(currActiveRoom);
  const setCurrActiveRoom = (data : Room | null) => {
    _setCurrActiveRoom(data);
    currActiveRoomRef.current = data;
  }

  const [activeRoomMessages, _setActiveRoomMessages] = useState<
    Message[] | null
  >([]); // active room messages list
  const activeRoomMessagesRef = useRef(activeRoomMessages);
  const setActiveRoomMessages = (data : Message[] | null) =>
  {
    _setActiveRoomMessages(data);
    activeRoomMessagesRef.current = data;
  }

  const [rooms, _setRooms] = useState<Room[] | null>(null); // all avail rooms
  const roomsRef = useRef(rooms);
  const setRooms = (data : Room[] | null) => {
    _setRooms(data);
    roomsRef.current = data;
  }

  const [notify, _setNotify] = useState<number[] | null>(null); // notified rooms
  const notifyRef = useRef(notify);
  const setNotify = (data : number[] | null) => 
  {
    _setNotify(data);
    notifyRef.current = data;
  }

  const [lastMessages, _setLastMessages] = useState<Message[] | null>(null); // last messages for all rooms
  const lastMessagesRef = useRef(lastMessages);
  const setLastMessages = (data : Message[] | null) =>
  {
    _setLastMessages(data);
    lastMessagesRef.current = data;
  }

  const [openAlert, setOpenAlert] = useState<AlertType>({
    type: "",
    isOpen: false,
  }); // open alert
  const [alertMessage, setAlertMessage] = useState<string>("");

  return (
    <ChatWidgetContext.Provider
      value={{
        currActiveRoom,
        setCurrActiveRoom,
        currActiveRoomRef,
        activeRoomMessages,
        setActiveRoomMessages,
        activeRoomMessagesRef,
        rooms,
        setRooms,
        roomsRef,
        notify,
        setNotify,
        notifyRef,
        lastMessages,
        setLastMessages,
        lastMessagesRef,
        openAlert,
        setOpenAlert,
        alertMessage,
        setAlertMessage,
      }}
    >
      {props.children}
    </ChatWidgetContext.Provider>
  );
};

export const useChatWidget = () => {
  return useContext(ChatWidgetContext);
};
