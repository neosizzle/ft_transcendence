import React, { createContext, useContext, useState } from "react";
import { Message, Room, SocketInterface } from "../Chat/classes";
import { AlertType } from "../commonComponents/Alert";

interface Props {
  children: React.ReactNode;
}

interface ChatWidgetCtx {
  currActiveRoom: Room | null;
  setCurrActiveRoom: React.Dispatch<React.SetStateAction<Room | null>>;
  activeRoomMessages: Message[] | null;
  setActiveRoomMessages: React.Dispatch<React.SetStateAction<Message[] | null>>;
  rooms: Room[] | null;
  setRooms: React.Dispatch<React.SetStateAction<Room[] | null>>;
  notify: number[] | null;
  setNotify: React.Dispatch<React.SetStateAction<number[] | null>>;
  lastMessages: Message[] | null;
  setLastMessages: React.Dispatch<React.SetStateAction<Message[] | null>>;
  socket: SocketInterface | null;
  setSocket: React.Dispatch<React.SetStateAction<SocketInterface | null>>;
  openAlert: AlertType;
  setOpenAlert: React.Dispatch<React.SetStateAction<AlertType>>;
  alertMessage : string;
  setAlertMessage : React.Dispatch<React.SetStateAction<string>>;
}

const ChatWidgetContext = createContext<ChatWidgetCtx | null>(null);

export const ChatWidgetProvider = (props: Props) => {
  const [currActiveRoom, setCurrActiveRoom] = useState<Room | null>(null); // active viewing room
  const [activeRoomMessages, setActiveRoomMessages] = useState<
    Message[] | null
  >(null); // active room messages list
  const [rooms, setRooms] = useState<Room[] | null>(null); // all avail rooms
  const [notify, setNotify] = useState<number[] | null>(null); // notified rooms
  const [lastMessages, setLastMessages] = useState<Message[] | null>(null); // last messages for all rooms
  const [socket, setSocket] = useState<SocketInterface | null>(null); // socket client interface
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
        activeRoomMessages,
        setActiveRoomMessages,
        rooms,
        setRooms,
        notify,
        setNotify,
        lastMessages,
        setLastMessages,
        socket,
        setSocket,
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
