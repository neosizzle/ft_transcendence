import React, {
  createContext,
  MutableRefObject,
  useContext,
  useRef,
  useState,
} from "react";
import { Admin, Member, Message, Room } from "../Chat/classes";
import { AlertType } from "../commonComponents/Alert";
import { Block } from "../User/pages/blocks/Blocks";
import { FriendShip } from "../User/pages/friends/Friends";
import { User } from "./authContext";

interface Props {
  children: React.ReactNode;
}

export interface ChatCtx {
  activeRoom: Room | null;
  activeRoomRef: MutableRefObject<Room | null>;
  setActiveRoom: (data: Room | null) => void;

  activeRoomCount: number;
  activeRoomCountRef: MutableRefObject<number>;
  setActiveRoomCount: (data: number) => void;

  activeRoomMessages: Message[] | null;
  activeRoomMessagesRef: MutableRefObject<Message[] | null>;
  setActiveRoomMessages: (data: Message[] | null) => void;

  activeRoomMessagesCount: number;
  activeRoomMessagesCountRef: MutableRefObject<number>;
  setActiveRoomMessagesCount: (data: number) => void;

  activeRoomBlocks: Block[];
  activeRoomBlocksRef: MutableRefObject<Block[]>;
  setActiveRoomBlocks: (data: Block[]) => void;

  activeRoomFriends: FriendShip[];
  activeRoomFriendsRef: MutableRefObject<FriendShip[]>;
  setActiveRoomFriends: (data: FriendShip[]) => void;

  memberUsers: User[] | null;
  memberUsersRef: MutableRefObject<User[] | null>;
  setMemberUsers: (data: User[] | null) => void;

  members: Member[] | null;
  membersRef: MutableRefObject<Member[] | null>;
  setMembers: (data: Member[] | null) => void;

  admins: Admin[];
  adminsRef: MutableRefObject<Admin[]>;
  setAdmins: (data: Admin[]) => void;

  openTimeModal: boolean;
  openTimeModalRef: MutableRefObject<boolean>;
  setOpenTimeModal: (data: boolean) => void;

  openConfirmationModal: boolean;
  openConfirmationModalRef: MutableRefObject<boolean>;
  setOpenConfirmationModal: (data: boolean) => void;

  userToAdminAction: User | null;
  userToAdminActionRef: MutableRefObject<User | null>;
  setUserToAdminAction: (data: User | null) => void;

  adminAction: string;
  adminActionRef: MutableRefObject<string>;
  setAdminAction: (data: string) => void;

  rooms: Room[] | null;
  roomsRef: MutableRefObject<Room[] | null>;
  setRooms: (data: Room[] | null) => void;

  openAlert: AlertType;
  setOpenAlert: React.Dispatch<React.SetStateAction<AlertType>>;

  alertMessage: string;
  setAlertMessage: React.Dispatch<React.SetStateAction<string>>;
}

const ChatContext = createContext<ChatCtx | null>(null);

export const ChatProvider = (props: Props) => {
  const [activeRoom, _setCurrActiveRoom] = useState<Room | null>(null); // active viewing room
  const activeRoomRef = useRef(activeRoom);
  const setActiveRoom = (data: Room | null) => {
    _setCurrActiveRoom(data);
    activeRoomRef.current = data;
  };

  const [activeRoomCount, _setActiveRoomCount] = useState<number>(0);
  const activeRoomCountRef = useRef(activeRoomCount);
  const setActiveRoomCount = (data: number) => {
    _setActiveRoomCount(data);
    activeRoomCountRef.current = data;
  };

  const [activeRoomMessages, _setActiveRoomMessages] = useState<
    Message[] | null
  >([]); // active room messages list
  const activeRoomMessagesRef = useRef(activeRoomMessages);
  const setActiveRoomMessages = (data: Message[] | null) => {
    _setActiveRoomMessages(data);
    activeRoomMessagesRef.current = data;
  };

  const [activeRoomMessagesCount, _setActiveRoomMessagesCount] =
    useState<number>(0); // active room messages list
  const activeRoomMessagesCountRef = useRef(activeRoomMessagesCount);
  const setActiveRoomMessagesCount = (data: number) => {
    _setActiveRoomMessagesCount(data);
    activeRoomMessagesCountRef.current = data;
  };

  const [activeRoomBlocks, _setActiveRoomBlocks] = useState<Block[]>([]); // active room blockss list
  const activeRoomBlocksRef = useRef(activeRoomBlocks);
  const setActiveRoomBlocks = (data: Block[]) => {
    _setActiveRoomBlocks(data);
    activeRoomBlocksRef.current = data;
  };

  const [openTimeModal, _setOpenTimeModal] = useState<boolean>(false); // open time selection modal
  const openTimeModalRef = useRef(openTimeModal);
  const setOpenTimeModal = (data: boolean) => {
    _setOpenTimeModal(data);
    openTimeModalRef.current = data;
  };

  const [openConfirmationModal, _setOpenConfirmationModal] =
    useState<boolean>(false); // open time selection modal
  const openConfirmationModalRef = useRef(openConfirmationModal);
  const setOpenConfirmationModal = (data: boolean) => {
    _setOpenConfirmationModal(data);
    openConfirmationModalRef.current = data;
  };

  const [userToAdminAction, _setUserToAdminAction] = useState<User | null>(
    null
  ); // user to take admin action against
  const userToAdminActionRef = useRef(userToAdminAction);
  const setUserToAdminAction = (data: User | null) => {
    _setUserToAdminAction(data);
    userToAdminActionRef.current = data;
  };

  const [adminAction, _setAdminAction] = useState<string>(""); // current admin action to execute
  const adminActionRef = useRef(adminAction);
  const setAdminAction = (data: string) => {
    _setAdminAction(data);
    adminActionRef.current = data;
  };

  const [activeRoomFriends, _setActiveRoomFriends] = useState<FriendShip[]>([]); // active room friendss list
  const activeRoomFriendsRef = useRef(activeRoomFriends);
  const setActiveRoomFriends = (data: FriendShip[]) => {
    _setActiveRoomFriends(data);
    activeRoomFriendsRef.current = data;
  };

  const [memberUsers, _setMemberUsers] = useState<User[] | null>(null); // current member users in room
  const memberUsersRef = useRef(memberUsers);
  const setMemberUsers = (data: User[] | null) => {
    _setMemberUsers(data);
    memberUsersRef.current = data;
  };

  const [members, _setMembers] = useState<Member[] | null>(null); // current members in room
  const membersRef = useRef(members);
  const setMembers = (data: Member[] | null) => {
    _setMembers(data);
    membersRef.current = data;
  };

  const [admins, _setAdmins] = useState<Admin[]>([]); // current admins in room
  const adminsRef = useRef(admins);
  const setAdmins = (data: Admin[]) => {
    _setAdmins(data);
    adminsRef.current = data;
  };

  const [rooms, _setRooms] = useState<Room[] | null>(null); // all avail rooms
  const roomsRef = useRef(rooms);
  const setRooms = (data: Room[] | null) => {
    _setRooms(data);
    roomsRef.current = data;
  };

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
        activeRoomCount,
        setActiveRoomCount,
        activeRoomCountRef,
        activeRoomMessages,
        setActiveRoomMessages,
        activeRoomMessagesRef,
        activeRoomMessagesCount,
        activeRoomMessagesCountRef,
        setActiveRoomMessagesCount,
        activeRoomBlocks,
        activeRoomBlocksRef,
        setActiveRoomBlocks,
        activeRoomFriends,
        activeRoomFriendsRef,
        setActiveRoomFriends,
        memberUsers,
        memberUsersRef,
        setMemberUsers,
        members,
        membersRef,
        setMembers,
        admins,
        adminsRef,
        setAdmins,
        openConfirmationModal,
        openConfirmationModalRef,
        setOpenConfirmationModal,
        userToAdminAction,
        userToAdminActionRef,
        setUserToAdminAction,
        adminAction,
        adminActionRef,
        setAdminAction,
        openTimeModal,
        openTimeModalRef,
        setOpenTimeModal,
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
