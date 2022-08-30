import React, { createContext, MutableRefObject, useContext, useRef, useState } from "react";
import { Message, Room } from "../Chat/classes";
import { AlertType } from "../commonComponents/Alert";

interface Props {
  children: React.ReactNode;
}

export interface ChatCtx {
  activeRoom: Room | null;
  activeRoomRef : MutableRefObject<Room | null>;
  setActiveRoom: (data : Room | null) => void;

  activeRoomMessages: Message[] | null;
  activeRoomMessagesRef : MutableRefObject <Message[] | null>;
  setActiveRoomMessages: (data : Message[] | null) => void;

  rooms: Room[] | null;
  roomsRef : MutableRefObject <Room[] | null>;
  setRooms: (data : Room[] | null) => void;

  openAlert: AlertType;
  setOpenAlert: React.Dispatch<React.SetStateAction<AlertType>>;

  alertMessage : string;
  setAlertMessage : React.Dispatch<React.SetStateAction<string>>;
}

const ChatContext = createContext<ChatCtx | null>(null);

export const ChatProvider = (props: Props) => {
  const [activeRoom, _setCurrActiveRoom] = useState<Room | null>(null); // active viewing room
  const activeRoomRef = useRef(activeRoom);
  const setActiveRoom = (data : Room | null) => {
    _setCurrActiveRoom(data);
    activeRoomRef.current = data;
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

  const [openAlert, setOpenAlert] = useState<AlertType>({
    type: "",
    isOpen: false,
  }); // open alert
  const [alertMessage, setAlertMessage] = useState<string>("");

  return (
    <ChatContext.Provider
      value={{
        activeRoom,
        setActiveRoom,
        activeRoomRef,
        activeRoomMessages,
        setActiveRoomMessages,
        activeRoomMessagesRef,
        rooms,
        setRooms,
        roomsRef,
        openAlert,
        setOpenAlert,
        alertMessage,
        setAlertMessage,
      }}
    >
      {props.children}
    </ChatContext.Provider>
  );
};

export const useChat = () => {
  return useContext(ChatContext);
};
