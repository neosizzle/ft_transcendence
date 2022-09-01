import React, { FunctionComponent, useEffect, useState } from "react";
import { useAuth, User } from "../../context/authContext";
import { ControlledMenu, MenuItem, useMenuState } from "@szhsin/react-menu";
import "@szhsin/react-menu/dist/index.css";
import { useChat } from "../../context/chatContext";
import { NavLink, useNavigate } from "react-router-dom";
import {
  ALREADY_FRIENDS,
  API_ROOT,
  NOT_FRIENDS,
  OUTGOING_GET_QUEUE,
  OUTGOING_INV,
  PENDING_FRIENDS,
} from "../../constants";
import { QueueInfo } from "../../Game/Canvas";
import { FriendShip } from "../../User/pages/friends/Friends";
import { auth_net_delete, auth_net_post } from "../../utils";

interface MemberCardProps {
  user: User;
  isAdmin: boolean;
  isOwner: boolean;
  isSelf: boolean;
  userBlocked: boolean;
  friendship?: FriendShip;
}

const MemberCard: FunctionComponent<MemberCardProps> = ({
  user,
  isAdmin,
  isOwner,
  isSelf,
  userBlocked,
  friendship,
}) => {
  const [menuProps, toggleMenu] = useMenuState();
  const [anchorPoint, setAnchorPoint] = useState({ x: 0, y: 0 });
  const [userBlockedLocal, setUserBlockedLocal] = useState<boolean>(userBlocked);
  const [friendshipStatus, setFriendshipStatus] = useState<number>(-1);
  const chat = useChat();
  const auth = useAuth();
  const navigate = useNavigate();

useEffect(() => {
  setFriendshipStatus(friendship
    ? friendship.reqStatus === "APPROVED"
      ? ALREADY_FRIENDS
      : friendship.reqStatus === "PENDING"
      ? PENDING_FRIENDS
      : NOT_FRIENDS
    : NOT_FRIENDS)
}, [friendship])

  
const blocksEndpoint = `${API_ROOT}/blocks`;
const friendsEndpoint = `${API_ROOT}/friends`;
  return (
    // Member card
    <div
      onContextMenu={(e) => {
        e.preventDefault();
        setAnchorPoint({ x: e.clientX, y: e.clientY });
        toggleMenu(true);
      }}
      key={user.id}
      className="border-2 text-center text-xl"
    >
      {`${user.username}`}

      {/* Context menu */}
      <ControlledMenu
        {...menuProps}
        anchorPoint={anchorPoint}
        onClose={() => toggleMenu(false)}
      >
        {/* View profile */}
        <MenuItem>
          <NavLink to={`/users/profile/${user.id}`}>View Profile</NavLink>
        </MenuItem>

        {/* Invite to queue */}
        {chat?.activeRoom?.type === "DM" && !isSelf ? (
          <MenuItem
            onClick={() => {
              // If user is not in queue, reject action
              auth?.gameSocket?.emit(OUTGOING_GET_QUEUE, (queue: QueueInfo) => {
                if (queue.position[0] === -1 && queue.position[1] === -1) {
                  chat?.setAlertMessage("User not in queue");
                  chat?.setOpenAlert({ type: "error", isOpen: true });
                  return;
                }

                //determines if invited to queue 0 or queue 1
                let queuePosition: number;
                if (queue.position[0] > -1) queuePosition = 1;
                else queuePosition = 0;

                auth?.chatWidgetSocket?.emit(
                  OUTGOING_INV,
                  JSON.stringify({
                    userId: user.id,
                    roomId: chat.activeRoom?.id,
                    queuePosition,
                  })
                );
              });
            }}
          >
            Invite to queue
          </MenuItem>
        ) : null}

        {/* Block user */}
        {!isSelf ? (
          <MenuItem onClick={()=>{
            // user is already blocked by you, unblock is enabled
            if (userBlockedLocal)
            auth_net_delete(`${blocksEndpoint}/${user.id}`).then(
              (data) => {
                if (data.error && data.error == "Forbidden")
                  return navigate("/logout");
                if (data.error) return alert("unblock err");
                setUserBlockedLocal(false);
              }
            );
          // else, block user
          else
            auth_net_post(`${blocksEndpoint}`, {
              id: user.id,
            }).then((data) => {
              if (data.error && data.error == "Forbidden")
                return navigate("/logout");
              if (data.error) return alert("block err");
              setUserBlockedLocal(true);
            });
          }}>{userBlockedLocal ? "Unblock" : "Block"}</MenuItem>
        ) : null}

        {/* Friend / unfriend */}
        {!isSelf ? (
          <MenuItem
          onClick={()=>{
            // do nothing if user friend request is pending
            if (friendshipStatus === PENDING_FRIENDS) return ;

             // if user is not friend, add friend
             if (friendshipStatus === NOT_FRIENDS)
             auth_net_post(`${friendsEndpoint}`, {
               id: user.id,
             }).then((data) => {
               if (data.error && data.error == "Forbidden")
                 return navigate("/logout");

                setFriendshipStatus(PENDING_FRIENDS)
               if (data.error) return alert("add friend error");
             });
           // else, delete friend
           else
             auth_net_delete(`${friendsEndpoint}/${user.id}`).then(
               (data) => {
                 if (data.error && data.error == "Forbidden")
                   return navigate("/logout");
                 if (data.error) return alert("remove friend error");
                 setFriendshipStatus(NOT_FRIENDS);
               }
             );
          }}
          >
            {friendshipStatus === NOT_FRIENDS ? "Add friend" : friendshipStatus === ALREADY_FRIENDS ? "Unfriend" : "Pending request"}
          </MenuItem>
        ) : null}

        {/* Mute */}
        {
          !isSelf 
        }

        {/* Ban */}

        {/* Transfer ownership */}

        {/* promote / demote */}

      </ControlledMenu>
    </div>
  );
};

export default MemberCard;
